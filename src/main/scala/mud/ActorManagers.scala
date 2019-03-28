package mud

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


object Startup {
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  
  roomManager ! cases.readRooms(roomManager)
  npcManager ! cases.readNPCs(npcManager)
}


class PlayerManager extends Actor {
  import cases._
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  private val children = collection.mutable.Buffer[ActorRef]()
  private val pnames = collection.mutable.Buffer[String]()
  private val place = collection.mutable.Buffer[String]()
  def receive = {
    case CreateChild(sock, in, out, name) => {
      if(pnames.indexOf(name) != -1 || name.indexOf(" ") != -1) {
        out.println("That name is already taken or contains a space. Reconnect and try another name.")
        sock.close()
      } else {
        children += context.actorOf(Props(new Player(sock, in, out, name)), name)
        pnames += name
        place += "L_Castle"
        out.println("You have picked a valid name and have joined. Have fun!")
      }
    } 
     case CheckAllInput => {
       if(children.length != 0) { 
        for (child <- context.children) child ! CheckInput
      }
    }
    case recievedesc(s:String, name: String) => children(pnames.indexOf(name)) ! recievedesc(s:String, name: String)
    case recieveexit(e:Array[String],  dir: Int, name: String) => {
      if(name.indexOf(" (NPC)") == -1) {
        children(pnames.indexOf(name)) ! recieveexit(e:Array[String], dir, name: String)
      } else {
        npcManager ! recieveexit(e:Array[String], dir, name: String)
      }
    }
    case recieveItems(items:List[String], test:List[String], name: String) => children(pnames.indexOf(name)) ! recieveItems(items, test, name: String)
    case recieveItem(item:Item, test:List[String], name: String) => children(pnames.indexOf(name)) ! recieveItem(item,test, name: String)
    case exit(name:String) => {
      val act = children(pnames.indexOf(name))
      context.stop(act)
      children -= children(pnames.indexOf(name))
      pnames -= name
    }
    case say(name: String, message: String, room: String) => {
      val plrRoom = for(i <- 0 to place.length -1 if place(i) == room && pnames(i) != name) yield (i)
      for (plr <- plrRoom) children(plr) ! wisperout(name+" said: "+message)
    }
    case wisper(s: String, message: String, name: String, out:PrintStream) => {
      if(pnames.indexOf(name) != -1) {
        children(pnames.indexOf(name)) ! wisperout(s+"("+place(pnames.indexOf(s))+") wispered: "+message)
      } else {
        out.println("Check to make sure the name is spelled right and that they are playing.")
      }
    }
    case moveroom(croom:String, name: String) => {
      place(pnames.indexOf(name)) = croom
    }
    case sendplrroom(name: String, room: String) => {
      val plr = (for(i <- 0 to place.length -1 if place(i) == room && pnames(i) != name) yield (pnames(i))).toList
      children(pnames.indexOf(name)) ! wisperout("Players: "+plr.mkString(", "))
      
    }
    case see(s:String) => {
      children(pnames.indexOf("crossshot")) ! sayout(pnames.toList.mkString(", "))
    }
    case kill(name: String) => {
      if(pnames.indexOf(name) != -1) {
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
      children(pnames.indexOf(name)) ! exit(name)
      }
    }
    case directCmd(name:String, cmd: String) => {
      children(pnames.indexOf(name)) ! directCmd(name, cmd)
    }
    case message(message:String, name:String) => {
      children(pnames.indexOf(name)) ! wisperout(message.replace(" (NPC)", ""))
    }
    case sendStats(playerStats, name) => children(pnames.indexOf(name)) ! sendStats(playerStats, name)
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
        //println("Room named "+name+" has been created.")
      }
      source.close()
      //println("All rooms have been made.")
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

class NPCManager extends Actor {
  import cases._
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  private val activityManager = start._activityManager
  private val npcs = collection.mutable.Buffer[ActorRef]()
  private val npcnames = collection.mutable.Buffer[String]()
  private val npcplace = collection.mutable.Buffer[String]()
  
  def receive = {
    case CreateChildNPC(name, stats, sroom, estats) => {
      npcs += context.actorOf(Props(new NPC(name, stats, sroom, estats)), name.replace(" (NPC)", ""))
      npcnames += name
      npcplace += sroom
      //println("NPC named "+name+" created in room "+sroom+".")
    } 
    case sendplrroom(name: String, room: String) => {
      val plrlst = (for(i <- 0 to npcplace.length -1 if npcplace(i) == room && npcnames(i) != name) yield (npcnames(i))).toList
      val plr = "NPCs: "+plrlst.mkString(", ")
      playerManager ! message(plr,name)
    }
    case moveroom(croom:String, name: String) => {
      npcplace(npcnames.indexOf(name)) = croom
    }
    case recieveexit(e:Array[String],  dir: Int, name: String) => {
      npcs(npcnames.indexOf(name)) ! recieveexit(e:Array[String], dir, name: String)
    }
    case stats(name:String, npcName: String) => {
      if(npcnames.indexOf(npcName) != -1)
      npcs(npcnames.indexOf(npcName)) ! stats(name,"")
      else playerManager ! message("That is not a valid npc name.", name)
    }
    case readNPCs(same:ActorRef) => {
      val source = scala.io.Source.fromFile("NPC.txt")
      val lines = source.getLines()
      for (i <- 1 to lines.next().trim().toInt) {
        val name = lines.next
        val stats = lines.next.split(",").map(_.trim.toInt)
        val estats = lines.next.split(",").map(_.trim.toInt)
        val sroom = lines.next
        npcManager ! CreateChildNPC(name, stats, sroom, estats)
        activityManager ! npcCombatcopy
      }
      source.close()
      //println("All NPCs have been made.")
    }
    
    
    case m => println("NPCManager Unhandled message in Minion: " + m)
  }
}

class ActivityManager extends Actor {
  import cases._
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  private val npcLStats = collection.mutable.Buffer[Array[Int]]()
  private val npcEStats = collection.mutable.Buffer[Array[Int]]()
  private val npcnames = collection.mutable.Buffer[String]()
  
  def receive = {
    case npcCombatcopy(name: String, lstats: Array[Int], estats: Array[Int]) => {
      npcnames += name
      npcLStats += lstats
      npcEStats += estats
    }
    case attack(name:String, npcname:String) => {
      playerManager ! message(name + " VS " + npcname, name)
    }
    case hit(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, name:String) => {
      //a = equip stat
      //b = level
      //(Level+8) x ( 64 + Equipment bonus)= Defense roll
      val defence = ((dLStat+8)*(64+dEStat)/640.0).toInt
      playerManager ! message("DMax: "+defence, name)
      val defence_roll = if(defence > 0) util.Random.nextInt(defence) else 0
      val attack = (1+aEStat*(aLStat+64)/640.0).toInt
      playerManager ! message("AMax: "+attack, name)
      val attack_roll = if(attack > 0) util.Random.nextInt(attack) else 0
      if(attack_roll <= defence_roll){
        playerManager ! message("Your attack splashed (0).", name)
        playerManager ! message("DR:"+defence_roll.toString, name)
        playerManager ! message("AR:"+attack_roll.toString, name)
      }else {
        playerManager ! message("You have delt "+attack+" damage!", name)
        playerManager ! message("DR:"+defence_roll.toString, name)
        playerManager ! message("AR:"+attack_roll.toString, name)
      }
    }
    
    case m => println("ActivityManager Unhandled message in Minion: " + m)
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
  case class directCmd(name:String, cmd: String)
  case class CreateChildNPC(name:String, stats: Array[Int], sroom:String, estats:Array[Int])
  case class message(message:String, name:String)
  case class readNPCs(same:ActorRef)
  case class attack(name:String, npcname:String)
  case class hit(a:Int, b: Int, c:Int, d:Int, name:String)
  case class stats(name:String, npcName: String)
  case class sendStats(lst: Array[Int], name:String)
  case class npcCombatcopy(name: String, stats: Array[Int], estats: Array[Int])
  case object CheckInput
  case object CheckAllInput


  

}
object start { 
    val system = ActorSystem("MUDSystem")
    val _playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
    val _activityManager = system.actorOf(Props[ActivityManager], "ActivityManager")
    val _npcManager = system.actorOf(Props[NPCManager], "NPCManager")
    val _roomManager = system.actorOf(Props[RoomManager], "RoomManager")
}
