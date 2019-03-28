package MUD

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import io.StdIn._
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import akka.actor.dungeon.Children
import scala.concurrent.duration._

class PlayerManager extends Actor {
  import cases._
  val roomManager = start._roomManager
  val playerManager = start._playerManager
  val children = collection.mutable.Buffer[ActorRef]()
  private val names = collection.mutable.Buffer[String]()
  private val place = collection.mutable.Buffer[String]()
  def receive = {
    case CreateChild(sock, in, out, name) => {
      if(names.indexOf(name) != -1 || name.indexOf(" ") != -1 ) {
        out.println("That name is already taken or contains a space. Reconnect and try another name.")
        sock.close()
      } else {
        children += context.actorOf(Props(new Player(sock, in, out, name)), name)
        names += name
        place += "L_Castle"
        out.println("Good choice.")
      }
    } 
     case CheckAllInput => {
       if(children.length != 0) { 
        for (child <- context.children) child ! CheckInput
      }
    }
    case recievedesc(s:String, name: String) => children(names.indexOf(name)) ! recievedesc(s:String, name: String)
    case recieveexit(e:Array[String],  dir: Int, name: String) => children(names.indexOf(name)) ! recieveexit(e:Array[String], dir, name: String)
    case recieveItems(items:List[String], test:List[String], name: String) => children(names.indexOf(name)) ! recieveItems(items, test, name: String)
    case recieveItem(item:Item, test:List[String], name: String) => children(names.indexOf(name)) ! recieveItem(item,test, name: String)
    case exit(name:String) => {
      val act = children(names.indexOf(name))
      context.stop(act)
      children -= children(names.indexOf(name))
      names -= name
    }
    case say(name: String, message: String, room: String) => {
      val plrRoom = for(i <- 0 to place.length -1 if place(i) == room && names(i) != name) yield (i)
      for (plr <- plrRoom) children(plr) ! sayout(name+" said: "+message)
    }
    case wisper(s: String, message: String, name: String, out:PrintStream) => {
      if(names.indexOf(name) != -1) {
        children(names.indexOf(name)) ! wisperout(s+" wispered: "+message)
      } else {
        out.println("Check to make sure the name is spelled right and that they are playing.")
      }
    }
    case moveroom(croom:String, name: String) => {
      place(names.indexOf(name)) = croom
    }
    case sendplrroom(name: String, room: String) => {
      val plr = (for(i <- 0 to place.length -1 if place(i) == room && names(i) != name) yield (names(i))).toList
      children(names.indexOf(name)) ! recieveplrroom(plr)
    }
    case see(s:String) => {
      children(names.indexOf("crossshot")) ! sayout(names.toList.mkString(", "))
    }
    case kill(name: String) => {
      if(names.indexOf(name) != -1) {
      children.foreach { _ ! wisperout(name+
          " has been deemed unworthy by King Lewis and has been punished with death! You will be teleported to Varrock Castle in the next 3 seconds to watch the execution.\n\n")
        }
      children.foreach { _ ! punished() }
      Thread.sleep(3000)
      playerManager ! say("Executioner", "The man found here known as by the of "+name+", has commited the crime of"+
                          "using (true = true), and not using tab! For these grievous crimes the punishment will be beheading!" + 
                          " May you be sent to the deapest parts of the scala api you filthy criminal!" +
                          "* The axe comes down with a loud thud and " +name+ "'s head is seen falling into a bloody bucket*", "Varrock_Castle")
      children.foreach { _ ! frees("") }
      children(names.indexOf(name)) ! exit(name)
      }
    }
    case m => println("PManager Unhandled message in Minion: " + m)
  }
}

class RoomManager extends Actor {
  import cases._
  val roomManager = start._roomManager
  val playerManager = start._playerManager
  private var _roomMap = Map[String, ActorRef]()
  def roomMap = _roomMap
  def receive = {
    case readRooms(same:ActorRef) => {
      val source = scala.io.Source.fromFile("Map.txt")
      val lines = source.getLines()
      for (i <- 1 to lines.next().trim().toInt) {
        val keyWord = lines.next
        val name = lines.next
        val desc = lines.next
        val items = List.fill(lines.next.trim.toInt) {
          Item(lines.next, lines.next, lines.next.toInt, lines.next.split(",").map(_.trim.toInt),
              lines.next.toBoolean, lines.next)
        }
        val exits = lines.next.split(",").map(_.trim)
        val enemy = Array.fill(lines.next.trim.toInt) {
          Enemy(lines.next, lines.next, lines.next.split(",").map(_.trim.toInt), Array.fill(lines.next.trim.toInt) {
            Item(lines.next, lines.next, lines.next.toInt, lines.next.split(",").map(_.trim.toInt),
                lines.next.toBoolean, lines.next)}, lines.next.toInt)
        }
        _roomMap = _roomMap + (keyWord -> context.actorOf(Props(new Room(name, desc, items, exits, enemy)), keyWord))
      }
      source.close()
    }
    case sendMap(roomMap: Map[String, ActorRef], who: ActorRef) => who ! recieveMap(_roomMap)
    case sendie(who: ActorRef) => roomManager ! sendMap(_roomMap, who) 
    case look(s:String) => roomMap(s) ! look
    case senddesc(s:String, name: String) => roomMap(s) ! senddesc(s, name: String)
    case sendexit(s:String, dir:Int, name: String) => roomMap(s) ! sendexit(s, dir, name: String)
    case sendItems(test:List[String], s: String, name: String) =>  roomMap(s) ! sendItems(test, s, name: String)
    case sendItem(itemName: String, itemAmount:Int, s: String, test:List[String], name: String) => roomMap(s) ! sendItem(itemName, itemAmount, s, test, name: String)
    case sendDropItem(item:Item, itemAmount:Int, s:String, testList:List[String]) => roomMap(s) ! sendDropItem(item, itemAmount, s, testList)
    
    case m => println("RManager Unhandled message in Minion: " + m)

  }
}

class ChatManager extends Actor {
  import cases._
  def receive = {
    
    case m => println("RManager Unhandled message in Minion: " + m)
  }
}

object cases {
  case class bigMap(keyword:String, name:String, desc:String, items:List[Item],
      exits:Array[String], enemy:Array[Enemy])
  case class processCommand(s: String)
  case class readRoomroom(lines: Iterator[String], same: ActorRef)
  case class readRooms(same:ActorRef)
  case class sendMap(roomMap: Map[String, ActorRef], who: ActorRef)
  case class recieveMap(roomMap: Map[String, ActorRef])
  case class sendie(who: ActorRef)
  case class recievedesc(s:String, name:String)
  case class look(s: String)
  case class senddesc(s:String, name: String)
  case class recieveexit(e:Array[String], dir: Int, name: String)
  case class sendexit(s:String, dir: Int, name: String)
  case class recieveItems(items:List[String], test:List[String], name: String)
  case class sendItem(itemName: String, itemAmount:Int = -1, s:String, testList:List[String], name: String)
  case class sendDropItem(item:Item, itemAmount:Int = -1, s:String, testList:List[String])
  case class recieveItem(item: Item, test:List[String], name: String)
  case class sendItems(test:List[String], s: String, name: String)
  case class CreateChild(sock: Socket, in: BufferedReader, out: PrintStream, name: String)
  case class exit(s:String)
  case class printMessage(message:String, i: Int, in: BufferedReader, out: PrintStream)
  case class wisper(s: String, message: String, name: String,out:PrintStream)
  case class wisperout(s: String)
  case class say(name: String, message: String, room: String)
  case class moveroom(croom:String, name: String)
  case class sayout(s:String)
  case class sendplrroom(name: String, room: String)
  case class recieveplrroom(lst: List[String])
  case class kill(name: String)
  case class see(s:String)
  case class frees(s:String)
  case class punished()
  case object CheckInput
  case object CheckAllInput


  

}
object start { 
    val system = ActorSystem("MUDSystem")
    val _playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
    val _roomManager = system.actorOf(Props[RoomManager], "RoomManager")
}
