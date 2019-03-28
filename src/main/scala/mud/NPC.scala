package mud

import akka.actor.Actor
import akka.actor.ActorRef
import cases._
import collection.mutable.Buffer

class NPC(name: String, npcstats: Array[Int], sroom: String, estats: Array[Int]) extends Actor{
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
  
  def receive = {
    case recieveexit(e:Array[String], dir: Int,name: String) => move(e, dir)
    case stats(name:String, npcName: String) => playerManager ! sendStats(playerStats, name)
    case m => {  
      println("NPC " + name + " Unhandled message in Minion: " + m)
    }
  }
  
  
  def processCommand(command: String): Unit = {
    val test = if(command.indexOf(" ") != -1) (command.split(" ").map(_.trim)).toList else List(command, "none")
    test(0) match {
        case "north" => roomManager ! sendexit(croom, 0, name)
        case "south" => roomManager ! sendexit(croom, 1, name)
        case "east" => roomManager ! sendexit(croom, 2, name)
        case "west" => roomManager ! sendexit(croom, 3, name)
        case "up" => roomManager ! sendexit(croom, 4, name)
        case "down" => roomManager ! sendexit(croom, 5, name)
        case "n" => roomManager ! sendexit(croom, 0, name)
        case "s" => roomManager ! sendexit(croom, 1, name)
        case "e" => roomManager ! sendexit(croom, 2, name)
        case "w" => roomManager ! sendexit(croom, 3, name)
        case "u" => roomManager ! sendexit(croom, 4, name)
        case "d" => roomManager ! sendexit(croom, 5, name)
        case "stab" => activityManager ! hit(equipStats(1),playerStats(1),equipStats(6),playerStats(6), name)
        case "slash" => activityManager ! hit(equipStats(2),playerStats(2),equipStats(7),playerStats(7), name)
        case "crush" => activityManager ! hit(equipStats(3),playerStats(3),equipStats(8),playerStats(8), name)
        case "magic" => activityManager ! hit(equipStats(4),playerStats(4),equipStats(9),playerStats(9), name)
        case "ranged" => activityManager ! hit(equipStats(5),playerStats(5),equipStats(10),playerStats(10), name)
    }
  }
  def move(exit: Array[String], dir: Int): Unit = {
     if(exit(dir) == "-1") {
       
       println("You can not go that way")
       } else {
       croom = exit(dir)
       npcManager ! cases.moveroom(croom, name)
     }
  }
}