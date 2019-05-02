package mud

import akka.actor.Actor
import akka.actor.ActorRef
import cases._
import collection.mutable.Buffer

class NPC(name: String, npcstats: Array[Int], sroom: String, way:(String,String), estats: Array[Int]) extends Actor{
  private val playerManager = start._playerManager
  private val roomManager = start._roomManager
  private val npcManager = start._npcManager
  private val activityManager = start._activityManager
  private var _roomMap = Map[String, ActorRef]()
  private val _name = name
  private var croom = sroom
  private var equipment = Buffer[Item]()
  private var playerStats = npcstats
  private var equipStats = estats
  private val preferedAttack = playerStats.indexOf(playerStats.max,playerStats.indexOf(playerStats.max)+1)
  private var hp = playerStats(0) + equipStats(0)
  private val directionsNTW = Map[Int, String](0 -> "north",1 -> "south", 2 -> "east", 3 -> "west", 4 -> "up", 5 -> "down")
  private val directionsWTN = Map[String, Int]("north" -> 0,"south" -> 1,"east" -> 2,"west" -> 3,"up" -> 4,"down" -> 5)
  private var hold = false
  private var first = true
  private var path = List[String]()
  
  //path from lumbrage to varrock 2,0,0,0,0
  def receive = {
    case directCmd(name: String, direction:Int) => {
       processCommand(directionsNTW(direction))
    }
    case recieveexit(e:Array[String], dir: Int,name: String) => move(e, dir)
    case stats(name:String, npcName: String) => playerManager ! sendStats(playerStats, name)
    case pullInfo(pname:String, npcName:String) => {
      npcManager ! npcCopy(pname, name, npcstats, sroom, estats)
      isHeld
    }
    case hit(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, atype:Int, name:String, npcName: String, who:String) => {
      activityManager ! hit(aEStat, aLStat, playerStats(atype+5), playerStats(atype+5), atype, name, npcName, who)
    }
    case hit2(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, atype:Int, name:String, npcName: String) => {
      playerManager ! hit(playerStats(1), playerStats(1), 0, 0, preferedAttack, name, npcName, "The enemies")
    }
    case hpCheck(name:String, npcName:String, pHP:Int, npcHP:Int) => {
      playerManager ! message("You have " +pHP+ " The enemy has "+hp+".", name)
    }
    case hpChange(damage: Int, name:String, npcName:String) => {
      playerManager ! message("Damage to enemy: "+damage, name)
      hp = hp - damage
      playerManager ! message("Enemy hp: "+hp,name)
      if(hp <= 0) {
        activityManager ! combatEnd(name:String, npcName:String, "The enemy was defeated!")
        playerManager ! hpreset(name:String)
      }
    }
    case isHeld(name:String) => isHeld
    
    case shortestPath(croom:String, name:String, nxtroom:String, path:List[String]) => {
      var numb = 0
        for(i <- path){ 
          activityManager ! AddRequest(numb + 20, _name, "move", directionsWTN(i))
          numb += 20
        }
      numb = 0
    }
    
    case sendie(name:String) => {
      move(Array("-1"), 0)
    }
    
    case m => {  
      println("NPC " + name + " Unhandled message in Minion: " + m)
    }
  }
  
  
  def processCommand(command: String): Unit = {
    if(!hold){
      val test = if(command.indexOf(" ") != -1) (command.split(" ").map(_.trim)).toList else List(command, "none")
      test(0) match {
          case "north" => roomManager ! sendexit(croom, 0, name+" (NPC)")
          case "south" => roomManager ! sendexit(croom, 1, name+" (NPC)")
          case "east" => roomManager ! sendexit(croom, 2, name+" (NPC)")
          case "west" => roomManager ! sendexit(croom, 3, name+" (NPC)")
          case "up" => roomManager ! sendexit(croom, 4, name+" (NPC)")
          case "down" => roomManager ! sendexit(croom, 5, name+" (NPC)")
          case "n" => roomManager ! sendexit(croom, 0, name+" (NPC)")
          case "s" => roomManager ! sendexit(croom, 1, name+" (NPC)")
          case "e" => roomManager ! sendexit(croom, 2, name+" (NPC)")
          case "w" => roomManager ! sendexit(croom, 3, name+" (NPC)")
          case "u" => roomManager ! sendexit(croom, 4, name+" (NPC)")
          case "d" => roomManager ! sendexit(croom, 5, name+" (NPC)")
      }
    }
  }
  def isHeld(): Unit = {
    if(hold) hold = false
    else hold = true
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
  
  
  def move(exit: Array[String], dir: Int): Unit = {
    if(first) {
      first = false
      if(way._1 != way._2) {
        roomManager ! shortestPath(croom, name+" (NPCs)", way._2, List[String]())
      }
    }
    if(way._1 != way._2) {
     if(exit(dir) == "-1") {
     } else {
       playerManager ! say(name+" (NPC)", name+" has left the room.", croom)
       croom = exit(dir) 
       playerManager ! say(name+" (NPC)", name+" has entered the room.", croom)
       npcManager ! moveroom(croom, name)
       if(croom == way._1) {
         roomManager ! shortestPath(croom, name+" (NPCs)", way._2, List[String]())
       } else if(croom == way._2){
         roomManager ! shortestPath(croom, name+" (NPCs)", way._1, List[String]())
       }
     }
    }
  }
}