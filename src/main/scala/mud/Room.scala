package mud

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import io.StdIn._
import cases._

class Room (
    name: String,
    desc: String,
    private var items: List[Item],
    exits: Array[String])extends Actor{
    val playerManager = start._playerManager
    val roomManager = start._roomManager
  
   def receive = {
    case senddesc(s:String, name: String) => playerManager ! recievedesc(description, name: String)
    case sendexit(s:String,  dir: Int, name: String) => playerManager ! recieveexit(exit(), dir, name: String)
    case sendItems(test:List[String], s:String, name: String) => playerManager ! recieveItems(itemNamesperRoom(), test, name: String)
    case sendDropItem(item:Item, itemAmount:Int, s:String) => dropItem(item, itemAmount)
    case sendItem(itemName: String, itemAmount:Int, s, test:List[String],name: String) => playerManager ! recieveItem(getItem(itemName, itemAmount), test, name: String)
    case m => println("Room Unhandled message in Minion: " + m)
  }
  
  def description(): String = { "\n" + name + "\n" + desc + "\nExits: " + 
    (for(i <- 0 to exits.length-1) yield {if(exits(i) == "-1") "" else direction(i)}).filter(_ != "").mkString(", ") + 
    "\nItems: " + (for(i <- 0 to items.length-1) yield {if(items(i).qty == 1) items(i).name else 
        items(i).name + "(" + items(i).qty + "X)"}).toList.mkString(", ")
   
  }
  
  def getItem(itemName: String, itemAmount: Int = -1): Item = {
     val inames = for(i <- 0 to items.length-1) yield {items(i).name}
     val check = inames.indexOf(itemName)
     if(itemAmount != -1 && items(check).qty > itemAmount){
       items = items.patch(check, Seq(items(check).copy(qty = items(check).qty - itemAmount)), 1)
       items(check)
     } else {
       val done = items(check)
       items = items.filter(_.name != itemName)
       done
     }
  }
    
  def dropItem(item: Item, itemAmount: Int = -1): Unit = {
     val inames = for(i <- 0 to items.length-1) yield {items(i).name}
     val check = inames.indexOf(item.name)
     if(inames.indexOf(item.name) != -1) 
       if(itemAmount != -1)
        items = items.patch(check, Seq(items(check).copy(qty = items(check).qty + itemAmount)), 1)
       else 
         items = items.patch(check, Seq(items(check).copy(qty = items(check).qty + item.qty)), 1)
     else {
       if(itemAmount != -1)
         items = items ::: List(item.copy(qty = itemAmount))
       else 
         items = items ::: List(item.copy(qty = item.qty))
     }
     
  }
  
  def itemNamesperRoom(): List[String] = {
    (for(i <- 0 to items.length-1) yield {items(i).name}).toList
  }
  
/*  def enemyNamesperRoom(): List[String] = {
    (for(i <- 0 to enemies.length-1) yield {enemies(i).name}).toList
*///  }
  
  def direction(i: Int): String = {
    i match {
      case 0 => "North"
      case 1 => "South"
      case 2 => "East"
      case 3 => "West"
      case 4 => "Up"
      case 5 => "Down"
    }
  }
  def exit():Array[String] = exits
}