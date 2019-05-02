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
import scala.collection._
import cases._
import akka.actor.Actor
import java.net.ServerSocket
import java.net.SocketException
import java.io.InputStreamReader
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PlayerManager extends MutableDLList with Actor {
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  private val children = new MutableDLList[ActorRef]()
  private val parties = new MutableDLList[mutable.Buffer[String]]()
  private val pnames = mutable.Buffer[String]()
  private val place = mutable.Buffer[String]()
  
  
  def receive = {
    case CreateChild(sock, in, out, _name) => {
      val name = _name.toLowerCase()
      if(pnames.indexOf(name) != -1 || name.indexOf(" ") != -1 || name.isEmpty()) {
        out.println("That name is already taken or contains a space. Reconnect and try another name.")
        sock.close()
      } else {
        var sroom = "L_Castle"
        if(name == "admin") sroom = "Server_Home"
        children += context.actorOf(Props(new Player(sock, in, out, name, sroom)), name)
        pnames += name
        place += sroom
        if(name != "admin") {
          out.println("You have picked a valid name and have joined. Have fun!")
          children(pnames.indexOf(name)) ! directCmd(name, 0)
        } else out.println("Type start to open networking")
      }
    } 
     case CheckAllInput => {
       if(children.length != 0) { 
        for (child <- context.children) child ! CheckInput
       }
    }
     case checkConnection() => {
       if(!children.isEmpty) {
         children.foreach {_ ! checkConnection() }
       }
     }
       
    case recievedesc(s:String, name: String) => children(pnames.indexOf(name)) ! recievedesc(s:String, name: String)
    case recieveexit(e:Array[String],  dir: Int, name: String) => {
      if(name.indexOf(" (NPC)") == -1) {
        children(pnames.indexOf(name)) ! recieveexit(e:Array[String], dir, name)
      } else {
        npcManager ! recieveexit(e:Array[String], dir, name.replace(" (NPC)", ""))
      }
    }
    case npccombatName(pname:String, name:String) => {
      if(pnames.length > 1)
        if(pname != "Console") {
          children(pnames.indexOf(pname)) ! npccombatName(pname:String, name:String)
        } else {
          for(plr <- children) plr ! npccombatName(pname, "none")
        }
    }
    case recieveItems(items:List[String], test:List[String], name: String) => children(pnames.indexOf(name)) ! recieveItems(items, test, name: String)
    case recieveItem(item:Item, test:List[String], name: String) => children(pnames.indexOf(name)) ! recieveItem(item,test, name: String)
    case exit(name:String) => {
      val act = children(pnames.indexOf(name))
      context.stop(act)
      children -= children(pnames.indexOf(name))
      place -= place(pnames.indexOf(name))
      pnames -= name
      
    }
    case hpreset(name:String) => {
      children(pnames.indexOf(name)) ! hpreset(name)
    }
    case say(name: String, message: String, room: String) => {
        if(pnames.indexOf(name) != -1) {
          val plrRoom = for(i <- 0 to place.length -1 if place(i) == room && pnames(i) != name) yield (i)
          for (plr <- plrRoom) children(plr) ! chatout(message)
        } else {
          val plrRoom = for(i <- 0 to place.length -1 if place(i) == room) yield (i)
          for (plr <- plrRoom) children(plr) ! chatout(message)
        }
    }
    case wisper(s: String, message: String, name: String) => {
      if(s == "Console" && name == "all") {
        for (plr <- children) plr ! wisperout(message)
      } else {
        if(pnames.indexOf(name) != -1) {
          children(pnames.indexOf(name)) ! wisperout(s+" whisper: "+message)
        } else {
          children(pnames.indexOf(s)) ! wisperout("Check to make sure the name is spelled right and that they are playing.")
        }
      }
    }
    case watch(name:String) => {
      if(pnames.indexOf(name) != -1) {
        children(pnames.indexOf(name)) ! watch(name:String)
      } else {
        children(pnames.indexOf("admin")) ! wisperout("Check to make sure the name is spelled right and that they are playing.")
      }
    }
    case hit(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, atype:Int, name:String, npcName: String, who:String) => {
      children(pnames.indexOf(name)) ! hit(aEStat, aLStat, dEStat, dLStat, atype, name, npcName, who)
    }
    case moveroom(croom:String, name: String) => {
      val plrRoomout = for(i <- 0 to place.length -1 if place(i) == place(pnames.indexOf(name)) && pnames(i) != name) yield (i)
      for (plr <- plrRoomout) children(plr) ! chatout(name+" has left the room.")
      place(pnames.indexOf(name)) = croom
      val plrRoomin = for(i <- 0 to place.length -1 if place(i) == croom && pnames(i) != name) yield (i)
      for (plr <- plrRoomin) children(plr) ! chatout(name+" has entered the room.")
    }
    case sendplrroom(name: String, room: String) => {
      val plr = (for(i <- 0 to place.length -1 if place(i) == room && pnames(i) != name) yield (pnames(i))).toList
      children(pnames.indexOf(name)) ! wisperout("Players: "+plr.mkString(", "))
      
    }
    case see(s:String) => {
      children(pnames.indexOf("admin")) ! wisperout(pnames.toList.mkString(", "))
    }
    case ban(name: String, reason:String) => {
      if(pnames.indexOf(name) != -1) {
        children.foreach { _ ! wisperout(name+" has been hit with the ban hammer!") }
        children(pnames.indexOf(name)) ! exit(name)
      }
    }
    case hpChange(damage: Int, name:String,npcName:String) => {
      children(pnames.indexOf(name)) ! hpChange(damage, name, npcName)
    }
    case message(message:String, name:String) => {
      if(pnames.indexOf(name) != -1)
      children(pnames.indexOf(name)) ! wisperout(message.replace(" (NPC)", ""))
    }
    case sendStats(playerStats, name) => children(pnames.indexOf(name)) ! sendStats(playerStats, name)
    case stopss() => if(pnames.length > 1) for(i <- 1 to children.length-1) children(i) ! exit(pnames(i))
    
    case createParty(name:String, p1name: String, p2name: String) => {
      val partyLst = parties.flatten
      val party = mutable.Buffer(name)
      if(partyLst.indexOf(name) == -1 && p1name != name && p2name != name) {
        for(i <- List(p1name, p2name)) {
          if(i != "" && i != "admin"){
            if(partyLst.indexOf(i) == -1) {
              playerManager ! message("You have been added to a party by "+name+".",i)
              party += i
            }
            else playerManager ! message(i+" is already in a party.",name)
          }
        } 
      } else playerManager ! message("You are already in a party and or you are trying to add yourself to your own party.",name)
      if(party.length > 1) parties += party
    }
    case partyLeave(name:String) => {
      val partyLst = parties.flatten
      val partySizes = (for(i <- parties) yield i.length).toList
      if(partyLst.indexOf(name) == -1) playerManager ! message("You are not in a party.", name)
      else {
        var possition = partyLst.indexOf(name)
        var numb = 0
        while(possition >= 0) {
          possition -= partySizes(numb)
          numb += 1
        }
        val possInLst = parties(numb-1).indexOf(name)
        parties(numb-1) -= name
        children(pnames.indexOf(name)) ! wisperout("You have left the party.")
        if(parties(numb-1).length == 1) {
          children(pnames.indexOf(parties(numb-1)(0))) ! wisperout(name+" has left the party. This was also the last other player in your party. It will be disbanded.")
          parties -= parties(numb-1)
        } else {
          for(i <- parties(numb-1)){
            children(pnames.indexOf(i)) ! wisperout(name+" has left the party.")
          }
        }
      }
    }
    
    case partyInvite(name:String, p1name:String) => {
      val partyLst = parties.flatten
      val partySizes = (for(i <- parties) yield i.length).toList 
      if(partyLst.indexOf(name) == -1) playerManager ! message("You are not in a party.",name)
      else {
        var possition = partyLst.indexOf(name)
        var numb = 0
        while(possition >= 0) {
          possition -= partySizes(numb)
          numb += 1
        }
        if(parties(numb).length >= 3) {
          playerManager ! message("Your party is already at max capacity.",name)
        } else if(partyLst.indexOf(p1name) == -1) {
          playerManager ! message("You have been added to a party by "+name+".",p1name)
          parties(numb) += p1name
        }else playerManager ! message(p1name+" is already in a party.",name)
      }
    }
    
    case psay(name: String, sends: String) => {
      val partyLst = parties.flatten
      val partySizes = (for(i <- parties) yield i.length).toList 
      if(partyLst.indexOf(name) == -1) playerManager ! message("You are not in a party.",name)
      else {
        var possition = partyLst.indexOf(name)
        var numb = 0
        while(possition >= 0) {
          possition -= partySizes(numb)
          numb += 1
        }
        for(i <- parties(numb-1)){
        children(pnames.indexOf(i)) ! wisperout(name+" said to the party: "+sends)
        }
      }
    }
    
    case party(name:String) => {
      val partyLst = parties.flatten
      val partySizes = (for(i <- parties) yield i.length).toList 
      if(partyLst.indexOf(name) == -1) playerManager ! message("You are not in a party.",name)
      else {
        var possition = partyLst.indexOf(name)
        var numb = 0
        while(possition >= 0) {
          possition -= partySizes(numb)
          numb += 1
        }
        playerManager ! message("Your party: "+parties(numb-1).mkString(", "),name)
      }
    }
    
    case shortestPath(croom:String, name:String, nxtroom:String, path:List[String]) => {
      children(pnames.indexOf(name)) ! shortestPath(croom, name, nxtroom, path)
    }
    
    case m => println("PManager Unhandled message in Minion: " + m)
  }
}

class RoomManager extends Actor {
  val roomManager = start._roomManager
  val playerManager = start._playerManager
  val npcManager = start._npcManager
  private var _roomMap = Map[String, ActorRef]()
  private var _roomnamesWTKW = Map[String, String]()
  private var _roomnamesNTW = Map[Int, String]()
  private var _roomnamesKWTN = Map[String,Int]()
  private var _roomExists = List[Array[String]]()
  private var _roomnamesNbTN = new BSTMap[Int, String](_ < _)
  private val directions = Map[Int, String](0 -> "north",1 -> "south", 2 -> "east", 3 -> "west", 4 -> "up", 5 -> "down")
  private var numb = 0
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
        val gap = lines.next
        _roomMap = _roomMap + (keyWord -> context.actorOf(Props(new Room(name, desc, items, exits)), keyWord))
        _roomnamesWTKW = _roomnamesWTKW + (name -> keyWord)
        _roomnamesNTW = _roomnamesNTW + (numb -> name)
        _roomnamesKWTN = _roomnamesKWTN + (keyWord -> numb)
        _roomnamesNbTN += (numb -> name)
        _roomExists = _roomExists ::: List(exits)
        numb += 1
      }
      source.close()
      println("All rooms have been made.")
    }
    case sendie(name:String) => {
      playerManager ! message(_roomnamesNbTN.mkString("\n"), name)
    }
    case moveroom(croom:String, name: String) => {
      val go = _roomnamesWTKW(_roomnamesNTW(croom.toInt))
      playerManager ! moveroom(go, name)
    }
    case senddesc(s:String, name: String) => roomMap(s) ! senddesc(s, name: String)
    case sendexit(s:String, dir:Int, name: String) => roomMap(s) ! sendexit(s, dir, name: String)
    case sendItems(test:List[String], s: String, name: String) =>  roomMap(s) ! sendItems(test, s, name: String)
    case sendItem(itemName: String, itemAmount:Int, s: String, test:List[String], name: String) => roomMap(s) ! sendItem(itemName, itemAmount, s, test, name: String)
    
    case shortestPath(croom:String, name:String, nxtroom:String, path:List[String]) => {
      if(name.indexOf(" (NPCs)") != -1) {
        if(reachable(croom, nxtroom,  _roomExists, _roomnamesKWTN))
          npcManager ! shortestPath(croom, name, nxtroom, shortestPathe(croom, nxtroom, _roomExists, directions, _roomnamesKWTN)._2)
      } else { 
      
        if(reachable(croom, _roomnamesWTKW(_roomnamesNTW(nxtroom.toInt)),  _roomExists, _roomnamesKWTN))
        playerManager ! message("You can get there by going: "+shortestPathe(croom, _roomnamesWTKW(_roomnamesNTW(nxtroom.toInt)), _roomExists, directions, _roomnamesKWTN)._2.mkString(", "),name)
        else playerManager ! message("You can not get to that room.",name)
      }
    }
    
    case sendDropItem(item:Item, itemAmount:Int, s:String) => roomMap(s) ! sendDropItem(item, itemAmount, s)
    case stopss() => {
      for(i <- 0 to numb-1) {
        val act = _roomMap(_roomnamesWTKW(_roomnamesNTW(i)))
        context.stop(act)
      }
      _roomMap = Map[String, ActorRef]()
      _roomnamesWTKW = Map[String, String]()
      _roomnamesNTW = Map[Int, String]()
      _roomnamesKWTN = Map[String, Int]()
      _roomnamesNbTN = new BSTMap[Int, String](_ < _)
      numb = 0
    }
    
    case m => println("RManager Unhandled message in Minion: " + m)

  }
  
  def reachable(node1:String,node2:String,connect:List[Array[String]],
      roomnames: Map[String, Int], visited: Set[String] = Set.empty):Boolean = {
    if(node1 == node2) true else {
      val newVisited = visited + node1
      var i = 0
      var ret = false
      while(!ret && i < 6) {
        val nroom = connect(roomnames(node1))(i)
        if( connect(roomnames(node1))(i) != "-1" && !visited(nroom)) ret ||= reachable(nroom, node2, connect, roomnames, newVisited)
        i += 1
      }
      ret
    }
  }
  
  def shortestPathe(node1:String, node2:String, connect:List[Array[String]],
      directions: Map[Int, String], roomnames: Map[String, Int],
      visited: Set[String] = Set.empty,theWay: List[String] = List.empty): (Int,List[String]) = {
    if(node1 == node2){
      (0, theWay)
    } else {
      var ret = (100000000, List[String]())
      val newVisited = visited + node1
      var i = 0
      while(i < 6) {
        val nroom = connect(roomnames(node1))(i)
        if( connect(roomnames(node1))(i) != "-1" && !visited(nroom)) {
          val _theWay = theWay ::: List(directions(i))
          val go = shortestPathe(nroom, node2, connect, directions, roomnames, newVisited, _theWay)
          var now = List[String]()
          if(ret._1 < go._1) now = ret._2
          else now = go._2
          ret = (ret._1 min go._1, now)
        }
        i += 1
      }
      val p = (ret._1 + 1, ret._2)
      p
    }
  }
}

class NPCManager extends Actor {
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  private val activityManager = start._activityManager
  private val npcs = mutable.Buffer[ActorRef]()
  private val npcnames = mutable.Buffer[String]()
  private val npcplace = mutable.Buffer[String]()
  private var numb = 0
  
  def receive = {
    case CreateChildNPC(name, stats, sroom, way, estats) => {
      val npc = context.actorOf(Props(new NPC(name, stats, sroom, estats, way)), name)
      npcs += npc
      npcnames += name
      npcplace += sroom
      npc ! sendie(name:String)
      //println("NPC named "+name+" created in room "+sroom+".")
    } 
    case sendplrroom(name: String, room: String) => {
      val plrlst = (for(i <- 0 to npcplace.length -1 if npcplace(i) == room && npcnames(i) != name) yield (npcnames(i))).toList
      val extras = plrlst.filter(!_.exists(Character.isDigit))
      val plr = "NPCs: "+extras.mkString(", ")
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
    case hpChange(damage: Int, name:String,npcName:String) => {
      npcs(npcnames.indexOf(npcName)) ! hpChange(damage, name, npcName)
    }
    case hit(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, stype:Int, name:String, npcName: String,who:String) => {
      npcs(npcnames.indexOf(npcName)) ! hit(aEStat, aLStat, dEStat, dLStat, stype, name, npcName, who)
    }
    case npcCopy(pname:String, name: String, npcstats: Array[Int], sroom: String, estats: Array[Int]) => {
      numb+=1
      npcManager ! CreateChildNPC(name+numb, npcstats, sroom+numb, estats, (sroom,sroom))
      playerManager ! npccombatName(pname,name+numb)
    }
    case hpCheck(name:String, npcName:String, pHP:Int, npcHP:Int) => {
      npcs(npcnames.indexOf(npcName)) ! hpCheck(name, npcName, pHP, npcHP)
    }
    case hit2(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, atype:Int, name:String, npcName: String) => {
      npcs(npcnames.indexOf(npcName)) ! hit2(aEStat, aLStat, dEStat, dLStat, atype, name, npcName)
    }
    case exit(name:String) => {
      val act = npcs(npcnames.indexOf(name))
      context.stop(act)
      if(name.exists(_.isDigit)) npcs(npcnames.indexOf(name.filter(!_.isDigit))) ! isHeld(name.filter(!_.isDigit))
      npcplace -= npcplace(npcnames.indexOf(name))
      npcs -= npcs(npcnames.indexOf(name))
      npcnames -= name
    }
    case pullInfo(pname:String, npcName:String) => {
      npcs(npcnames.indexOf(npcName)) ! pullInfo(pname, npcName)
    }
    case directCmd(name:String,direction:Int) => {
      if(npcnames.indexOf(name) != -1){
        npcs(npcnames.indexOf(name)) ! directCmd(name,direction)
      }
    }
    case readNPCs(same:ActorRef) => {
      val source = scala.io.Source.fromFile("NPC.txt")
      val lines = source.getLines()
      for (i <- 1 to lines.next().trim().toInt) {
        val name = lines.next
        val stats = lines.next.split(",").map(_.trim.toInt)
        val estats = lines.next.split(",").map(_.trim.toInt)
        val sroom = lines.next
        val way = (lines.next, lines.next)
        val gap = lines.next
        npcManager ! CreateChildNPC(name, stats, sroom, estats, way)
      }
      source.close()
      println("All NPCs have been made.")
    }
    case stopss() => {
      for(i <- 0 to npcs.length-1) npcManager ! exit(npcnames(i))
    }
    case shortestPath(croom:String, name:String, nxtroom:String, path:List[String]) => {
      npcs(npcnames.indexOf(name.replace(" (NPCs)", ""))) ! shortestPath(croom, name.replace(" (NPCs)", ""), nxtroom, path)
    }
    case m => println("NPCManager Unhandled message in Minion: " + m)
  }
}

class ActivityManager extends Actor {
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  private val activityManager = start._activityManager
  private var ss:ServerSocket = null
  //private var priorityQueue = new SortedLLPQ[(Int,String, String, Int)](_._1 < _._1)
  private var priorityQueue = new BinaryHeapPQ[(Int,String, String, Int)](_._1 < _._1)
  private var numb = 0
  private var stop = true
  
  def receive = {
    
    case AddRequest(rank:Int, name:String, command:String, int:Int) => {
      if(!stop) {
        priorityQueue.enqueue((rank+numb, name, command, int))
      }
    }
    
    case CheckRequest() => {
      if(!priorityQueue.isEmpty){
        val time = priorityQueue.peek._1
        if(time <= numb) activityManager ! RemoveRequest()
        numb += 1
      }
    }
    
    case RemoveRequest() => {
      if(!priorityQueue.isEmpty) {
        val name = priorityQueue.peek._2
        val cmd = priorityQueue.peek._3
        val dir = priorityQueue.peek._4
        priorityQueue.dequeue()
        cmd match {
          //On Start Up
          case "room" => roomManager ! readRooms(roomManager)
          case "npc" => npcManager ! readNPCs(npcManager)
          case "Ready" => println("Server is now open. Have Fun!")
          
          //After Start Up
          case "move" => {
            npcManager ! directCmd(name,dir)
          }
          
          //End
          case "clearNPC" => npcManager ! stopss()
          case "clearPlayer" => playerManager ! stopss()
          case "clearRoom" => roomManager ! stopss()
          case "clearCombat" => playerManager ! npccombatName(name, "none")
          case "clearQueue" => activityManager ! clearQueue()
          case "closeNetwork" => ss.close()
          case _ => println("Priority Queue could not find case for: "+cmd)
        }
      }
    }
    
    case clearQueue() => {
      stop = true
      while(!priorityQueue.isEmpty) {
        priorityQueue.dequeue()
      }
      println("To restart the network just enter start again.")
    }
    
    case startup(_ss:ServerSocket) => {
      stop = false
      priorityQueue.enqueue((0, "Console","room", 0))
      priorityQueue.enqueue((0, "Console","npc", 0))
      priorityQueue.enqueue((0, "Console","Ready", 0))
      //priorityQueue.enqueue((10, "herald", "move", 2))
      ss = _ss
    }
    
    case stopss() => {
      priorityQueue.enqueue((0, "Console","clearCombat", 0))
      priorityQueue.enqueue((15, "Console","clearPlayer", 0))
      priorityQueue.enqueue((20, "Console","clearNPC", 0))
      priorityQueue.enqueue((30, "Console", "clearRoom", 0))
      priorityQueue.enqueue((40, "Console", "closeNetwork", 0))
      priorityQueue.enqueue((50, "Console", "clearQueue", 0))
      /*val sock = new Socket("localhost",4311)
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      val name = "Stop The Server Now"
      out.println(name)
      val sock2 = new Socket("localhost",4311)*/
    }
    
    case attack(name:String, npcname:String) => {
      playerManager ! message(name + " has entered combat with " + npcname + ".", name)
      npcManager ! pullInfo(name, npcname)
    }
    case hit(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, stype:Int, name:String, npcName: String, who:String) => {
      //a = equip stat
      //b = level
      //(Level+8) x ( 64 + Equipment bonus)= Defense roll
      val defence = ((dLStat+8)*(64+dEStat)/640.0).toInt
      //playerManager ! message("DMax: "+defence, name)
      val defence_roll = if(defence > 0) util.Random.nextInt(defence+1) else 0
      val attack = (1+aEStat*(aLStat+64)/640.0).toInt
      //playerManager ! message("AMax: "+attack, name)
      val attack_roll = if(attack > 0) util.Random.nextInt(attack+1) else 0
      if(attack_roll <= defence_roll){
        playerManager ! message(who+" attack splashed (0).", name)
        //playerManager ! message("DR:"+defence_roll.toString, name)
        //playerManager ! message("AR:"+attack_roll.toString, name)
      }else {
        playerManager ! message(who+" attack delt "+attack+" damage!", name)
        if(who == "Your") {
          npcManager ! hpChange(attack, name, npcName)
        }else {
          playerManager ! hpChange(attack, name, npcName)
      }
        //playerManager ! message("DR:"+defence_roll.toString, name)
        //playerManager ! message("AR:"+attack_roll.toString, name)
      }
    }
    case combatEnd(name:String, npcName:String, outcome:String) => {
      if(npcName != "none") {
        npcManager ! exit(npcName)
        playerManager ! message(outcome, name)
      }
    }
    case m => println("ActivityManager Unhandled message in Minion: " + m)
  }
}
object cases {
  case class readRooms(same:ActorRef)
  case class sendie(name:String)
  case class recievedesc(s:String, name:String)
  case class senddesc(s:String, name: String)
  case class recieveexit(e:Array[String], dir: Int, name: String)
  case class sendexit(s:String, dir: Int, name: String)
  case class recieveItems(items:List[String], test:List[String], name: String)
  case class sendItem(itemName: String, itemAmount:Int = -1, s:String, testList:List[String], name: String)
  case class sendDropItem(item:Item, itemAmount:Int = -1, s:String)
  case class recieveItem(item: Item, test:List[String], name: String)
  case class sendItems(test:List[String], s: String, name: String)
  case class CreateChild(sock: Socket, in: BufferedReader, out: PrintStream, name: String)
  case class exit(s:String)
  case class wisper(s: String, message: String, name: String)
  case class wisperout(s: String)
  case class say(name: String, message: String, room: String)
  case class moveroom(croom:String, name: String)
  case class sendplrroom(name: String, room: String)
  case class ban(name: String, reason:String)
  case class see(s:String)
  case class frees(s:String)
  case class directCmd(name:String, direction:Int)
  case class CreateChildNPC(name:String, stats: Array[Int],sroom:String, estats:Array[Int],way:(String,String))
  case class message(message:String, name:String)
  case class readNPCs(same:ActorRef)
  case class attack(name:String, npcname:String)
  case class hit(a:Int, b: Int, c:Int, d:Int, atype:Int, name:String, npcName: String, who:String)
  case class hit2(a:Int, b: Int, c:Int, d:Int, atype:Int, name:String, npcName: String)
  case class stats(name:String, npcName: String)
  case class sendStats(lst: Array[Int], name:String)
  case class pullInfo(pname:String, npcName:String)
  case class npcCopy(pname:String, name: String, npcstats: Array[Int], sroom: String, estats: Array[Int])
  case class npccombatName(pname:String, name:String)
  case class hpChange(damage: Int, name:String, npcName:String)
  case class combatEnd(name:String, npcName:String, outcome:String)
  case class hpCheck(name:String, npcName:String, pHP:Int, npcHP:Int)
  case class hpreset(name:String)
  case class chatout(s:String)
  case class watch(name:String)
  case class checkConnection()
  case class AddRequest(rank:Int, name:String, command:String, int:Int)
  case class RemoveRequest()
  case class CheckRequest()
  case class startup(_ss:ServerSocket)
  case class startss()
  case class stopss()
  case class clearQueue()
  case class createParty(name:String, p1name: String, p2name: String)
  case class partyLeave(name:String)
  case class psay(name: String, message: String)
  case class isHeld(name:String)
  case class partyInvite(name:String, p1name:String)
  case class party(name:String)
  case class shortestPath(croom:String, name:String, nxtroom:String, path:List[String])
  case object CheckInput
  case object CheckAllInput
  
}

class Server extends Actor{
  private val roomManager = start._roomManager
  private val playerManager = start._playerManager
  private val npcManager = start._npcManager
  private val activityManager = start._activityManager
  private val server = start._server
  private val system = start.system
  private var accepts = true
  private var onLine = false
  
  def receive = {
    
    case startss() => {
      val ss = new ServerSocket(4311)
      accepts = true
      activityManager ! cases.startup(ss)
      try {
        while(true) {
          val sock = ss.accept()
          val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
          val out = new PrintStream(sock.getOutputStream)
          Future {
            out.println("What is your name? (No spaces)")
            val name = in.readLine()
              playerManager ! cases.CreateChild(sock, in, out, name)
          }
        }
      } catch {
        case e: SocketException => println("Network has been closed.")
        case _: Throwable => println("Got some other kind of exception found in server.")
      }
    }
    
    case m => println("Server Unhandled message in Minion: " + m)
  }
}

object start { 
    val system = ActorSystem("MUDSystem")
    val _playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
    val _server = system.actorOf(Props[Server], "Server")
    val _activityManager = system.actorOf(Props[ActivityManager], "ActivityManager")
    val _npcManager = system.actorOf(Props[NPCManager], "NPCManager")
    val _roomManager = system.actorOf(Props[RoomManager], "RoomManager")
}
