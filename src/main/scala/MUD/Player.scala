package MUD

import collection.mutable.Buffer
import akka.actor.Actor
import akka.actor.ActorRef
import cases._
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import scala.io.StdIn._


class Player (sock: Socket, in: BufferedReader, out: PrintStream, name: String) extends Actor{
  def receive = {
    case recieveMap(roomMap: Map[String, ActorRef]) => plzWork(roomMap)
    case s:String => processCommand(s)
    case recievedesc(s:String, name: String) => {
      out.println(s)
      playerManager ! cases.sendplrroom(name, croom)
    }
    case recieveexit(e:Array[String], dir: Int,name: String) => move(e, dir)
    case recieveItems(items:List[String], test:List[String], name: String) => {
      if (items.indexOf(test(1)) == -1) { 
        
        out.println("That item is not in the room.")
      }
      else if(test.length -1 == 2)
        roomManager ! sendItem(test(1), test(2).toInt, croom, test, name)
        else 
          roomManager ! sendItem(test(1),-1, croom, test, name)
    }
    case recieveItem(item:Item, test:List[String], name: String) => addToInventory(item)
    case punished() => punishment()
    case exit(name:String) => playerManager ! exit(name)
    case CheckInput =>{
      if(in.ready()) {
        val input = in.readLine()
        if(input != "exit") {
         processCommand(input) 
        }
        else playerManager ! exit(name)
      }
    }
    case wisperout(s:String) => {
      out.println(s)
    }
    case frees(s:String) => free()
    case sayout(s:String) => {
      out.println(s)
    }
    case recieveplrroom(lst: List[String]) => out.println("Players: "+lst.mkString(", "))
    case m => {
      
      out.println("player Unhandled message in Minion: " + m)
    }
  }
  private val playerManager = start._playerManager
  private val roomManager = start._roomManager
  private var _roomMap = Map[String, ActorRef]()
  private val combat = new Combat
  private var croom = "L_Castle" 
  private var inventory = Buffer[Item]()
  private var inventoryNumber = (for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
      else inventory(i).qty}).sum
  private var equipment = Buffer[Item]()
  private val _name = name
  private var punish = false
  //Player Stats key: 
  //  1) Hp
  //  2-5) Attack: Stab, Slash, Crush, Magic, Ranged
  //  6-10 Defence: Stab, Slash, Crush, Magic, Ranged
  private var playerStats = Buffer[Int](15,1,1,1,1,1,1,1,1,1,1)
  private var equipStats = Buffer[Int](0,0,0,0,0,0,0,0,0,0,0)
  private var kit = true
  private var sinv = true
  def plzWork(roomMap: Map[String, ActorRef]): Unit = {
    val _roomMap = roomMap
  }
  
      
  def processCommand(command: String): Unit = {
    if(punish != true) {
    val test = if(command.indexOf(" ") != -1) (command.split(" ").map(_.trim)).toList else List(command, "none")
    test(0) match {
        case "sinv" => if(sinv == true) {
          for ( i <- sItems) addToInventory(i)
          sinv = false
        } else {
          out.println("That is not a command. Type Help for a list of valid commands.")
        }
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
        case "get" => if(inventoryNumber >= 28) {
          
          out.println("Your inventory is full.")
        }
            else { 
              roomManager ! sendItems(test, croom, name)
            }
        case "drop" => if(itemListInventory().indexOf(test(1)) == -1) {
          
          out.println("That item is not in your inventory.")
        }
            else if(test.length -1 == 2) roomManager ! sendDropItem(getFromInventory(test(1), test(2).toInt), test(2).toInt, croom, test)
            else roomManager ! sendDropItem(getFromInventory(test(1), -1), -1, croom, test)
        case "look" => { 
            roomManager ! cases.senddesc(croom, name)
          }
        case "inv" => {
          out.println(inventoryListing)
        }
        case "inventory" =>{
          out.println(inventoryListing)
        }
        case "help" => help()
        case "inspect" => inspectItem(test(1))
        case "equip" => if(itemListInventory().indexOf(test(1)) == -1){
          
          out.println("That item is not in your inventory.")
        }
            else equipItem(getFromInventory(test(1), 1))
        case "eview" => {
          out.println(equipListing())
        }
        case "unequip" => { 
          if(itemListEquipment().indexOf(test(1)) == -1) {
            
            out.println("That item is not equiped.")
          } else addToInventory(unequipItem(test(1)))
        }
        case "level" => {
          out.println(statView(playerStats))
        }
        case "stats" => out.println(statView(equipStats))
        case "lewis" => if(kit) for(i <- tItems) equipItem(i) else {
          
          out.println("That is not a command. Type Help for a list of valid commands.")
        }
        //case "attack" => if(Room.rooms(croom).enemyNamesperRoom.indexOf(test(1)) == -1) out.println("That enemy is not in the room.", 1, in, out)
        //    else combat(Room.rooms(croom).enemy(test(1)))
        //case "enemy_stats" => if(Room.rooms(croom).enemyNamesperRoom.indexOf(test(1)) == -1) out.println("That enemy is not in the room.", 1, in, out)
        //     else out.println(Room.rooms(croom).enemy(test(1)).stats.mkString(", ")), 1, in, out)
        case "exit" => {
          out.println("Th-Th-The, Th-Th-The, Th-Th... That's all, folks!")
        }
        case "say" => {
          if (test.length -1 > 1)
            playerManager ! say(name,(for(i <- test if i != test(0)) yield (i)).toList.mkString(" "), croom)
           else playerManager ! say(name, test(1), croom)
        }
          case "wisper" => {
          playerManager ! wisper(name, (for(i <- test if i != test(0) && i != test(1)) yield (i)).toList.mkString(" "), test(1), out)
        }
          case "serverhome" => {
            if(name == "crossshot") {
              hub()
            } else processCommand("no")
          }
          case "see" => {
            if(croom == "Server_Home") {
              playerManager ! see(" ")
            }else processCommand("no")
          }
          case "kill" => {
            if(croom == "Server_Home") {
              playerManager ! kill(test(1))
              
            }else processCommand("no")
          }
        case _ => {
          out.println("That is not a command. Type Help for a list of valid commands. " + test(1))
        }
    }
    }
  }
  
  def getFromInventory(itemName: String, itemAmount: Int = -1): Item = {
     val inames = itemListInventory()
     val check = inames.indexOf(itemName)
     if(inventory(check).qty > 1) {
       if(itemAmount != -1) {
         inventory = inventory.patch(check, Seq(inventory(check).copy(qty = inventory(check).qty - itemAmount)), 1)
         inventoryNumber = (for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
           else inventory(i).qty}).sum
         inventory(check)
       } else {
       val itm = inventory(check)
       inventory = inventory.filter(_.name != itemName)
       inventoryNumber = (for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
         else inventory(i).qty}).sum
       itm
       }
     } else { 
       val itm = inventory(check)
       inventory = inventory.filter(_.name != itemName)
       inventoryNumber = (for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
         else inventory(i).qty}).sum
       itm
     }
     
  }
  
  def playerStat = playerStats
  
  def addToInventory(item: Item): Unit = {
    val inames = itemListInventory
    
    out.println("The item: " + item.name + " has been added to your inventory!")
    if(inames.indexOf(item.name) != -1) {
       val check = inames.indexOf(item.name)
       inventory = inventory.patch(check, Seq(inventory(check).copy(qty = inventory(check).qty + item.qty)), 1)
    } else {
      inventory = inventory += item
    }
    inventoryNumber = (for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
      else inventory(i).qty}).sum
  }
  
  def inventoryListing(): String = {
   val space = ((for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
      else inventory(i).qty}).sum + "/28")
    val itms = if(inventory.length == 0) "You have no items in your inventory." 
      else {
      (for(i <- 0 to inventory.length-1) yield {inventory(i).name + "(" + 
        inventory(i).qty + "X) - " + inventory(i).desc}).toList.mkString("\n")
      }
   space + "\n" + itms
  }
  
  def itemListInventory(): List[String] = {
    (for(i <- 0 to inventory.length-1) yield {inventory(i).name}).toList
  }
  
  def itemListEquipment(): List[String] = {
    (for(i <- 0 to equipment.length-1) yield {equipment(i).name}).toList
  }
  
  def itemListEquipmentslot(): List[String] = {
    (for(i <- 0 to equipment.length-1) yield {equipment(i).slot}).toList
  }
  def free(): Unit = {
    punish = false
  }
  def punishment(): Unit = {
       punish = true
       croom = "Varrock_Castle"
       playerManager ! cases.moveroom(croom, name)
  }
  def hub(): Unit = {
       croom = "Server_Home"
       playerManager ! cases.moveroom(croom, name)
  }
  
  def move(exit: Array[String], dir: Int): Unit = {
     if(exit(dir) == "-1") {
       
       out.println("You can not go that way")
       } else {
       croom = exit(dir)
       roomManager ! cases.senddesc(croom, name)
       playerManager ! cases.moveroom(croom, name)
       kit = false
     }
  }
     
  def help(): Unit = {
    val help = ("""north, south, east, west, up, down - For movement (it is nice if you allow single letter abbreviations)
look - Reprints the description of the current room
inv/inventory - List the contents of your inventory
get *item *amount - To get an item from the room and add it to your inventory. Default for amount is all.
drop *item *amount - To drop an item from your inventory into the room. Default for amount is all.
exit - Leave the game
help - Print the available commands and what they do
inspect *item - Looks at item's information that is located inside your inventory.
WIP attack *enemy - Enter combat with enemy.
eview - Looks at the items currently equiped.
level - Looks at player stats.
stats - Looks at player's equipment stats.
equip *item - Adds item to players active equipment.
unequip *item - Removes item from players active equipment.
WIP enemy_stats - Looks at an enemie's stats.
wisper *player name *message - Sends a private massage to a player.
say *message - say something to all players in your current room")
""")
  out.println(help)
  }
  
  def inspectItem(itemName: String): Unit = {
    val ilnames = itemListInventory()
    val ienames = itemListEquipment()
    if(ilnames.indexOf(itemName) == -1 && ienames.indexOf(itemName) == -1) {
      
      out.println("You do not have that item inside your inventory or equiped.")
    }
    else if(itemName == "gold") {
      val check = ilnames.indexOf(itemName)
      val done = inventory(check)
      out.println("Name: " + done.name + "\nQuantity: " + done.qty + 
          "\nDescription: " + done.desc)
    } else {
      if(ilnames.indexOf(itemName) != -1) {
        val check = ilnames.indexOf(itemName)
        val done = inventory(check)
        out.println("Name: " + done.name + "\nStats: " + done.stats.mkString(", ") + "\nQuantity: " + done.qty + 
          "\nDescription: " + done.desc  + "\nEquipable: " + done.equipable + (if(done.equipable)
            "\nSlot: " + done.slot).toString)
      } else {
        val check = ienames.indexOf(itemName)
        val done = equipment(check)
        out.println("Name: " + done.name + "\nStats: " + done.stats.mkString(", ") + "\nQuantity: " + done.qty + 
          "\nDescription: " + done.desc  + "\nEquipable: " + done.equipable + (if(done.equipable)
            "\nSlot: " + done.slot).toString)
      }
    }
  }
  
  def equipListing(): String = {
    if(equipment.length == 0) "You have no items currently equiped." else 
      (for(i <- 0 to equipment.length-1) yield {equipment(i).name + " - " +
        equipment(i).desc}).toList.mkString("\n")
  }
  
  def equipItem(item: Item): Unit = {
    val islot = itemListEquipmentslot()
    if(islot.length-1 == -1) {
      equipment += item
      statsUpdate(item.stats)
      
      out.println("The item has been equiped")
    } else {
      if(item.equipable && islot.indexOf(item.slot) == -1) {
          equipment += item
          statsUpdate(item.stats)
          
          out.println("The item has been equiped")
      } else { 
        
        out.println("You already have an item equiped in that slot.")
        addToInventory(item)
    }
    }
  }
  
  def unequipItem(itemName: String): Item = {
    val iname = itemListEquipment()
    val check = iname.indexOf(itemName)
    val done = equipment(check)
    statsUpdate(done.stats.map(_ * -1))
    equipment -= equipment(check)
    done
  }
  
  def statsUpdate(stats: Array[Int]): Unit = {
    val pls = equipStats
    val its = stats
    equipStats = (for(i <- 0 to 10) yield {pls(i)+its(i)}).toBuffer
  }
  
  def statView(lst: Buffer[Int]): String = {
    val ps = lst
    val hp = "HitPoints: " + ps(0)
    val aStab = "Stab Attack: " + ps(1)
    val aSlash = "Slash Attack: " + ps(2)
    val aCrush = "Crush Attack: " + ps(3)
    val aMagic = "Magic Attack: " + ps(4)
    val aRanged = "Ranged Attack: " + ps(5)
    val dStab = "Stab Defence: " + ps(6)
    val dSlash = "Slash Defence: " + ps(7)
    val dCrush = "Crush Defence: " + ps(8)
    val dMagic = "Magic Defence: " + ps(9)
    val dRanged = "Ranged Defence: " + ps(10)
    hp + "\n" + "		Attack" + "\n" + aStab + "	" + aSlash + "	" + 
      aCrush + "\n" + aMagic + "	" + aRanged + "\n" + "		Defence" + "\n" + 
      dStab + "	" + dSlash + "	" + dCrush + "\n" + dMagic + "	" +
      dRanged + "\n"
  }
  
  def combat(enemy: Enemy): Unit = {
    var fow = enemy
    val reset = fow.stats(0)
    var yous = playerStats
    var youe = equipStats
    out.println("You have entered combat with a " + enemy.name + "!", 1, in, out)
    while(fow.stats(0) >= 0 && yous(0) >= 0) {
      out.println("Your HP: " + yous(0) + "			" + enemy.name + " HP: " + fow.stats(0))
      out.println("What attack would you like to use? \n", 1, in, out)
      out.println("Stab, Slash, Crush, Magic, or Ranged?", 1, in, out)
      val attack = readLine.toLowerCase().trim()
      val damagedelt = 1 //damage(youe(1), yous(1))
      val damagetaken = 1 //damage
      attack match {
        case "stab" => out.println("You have done " + damagedelt * yous(1) / enemy.stats(6) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * yous(1) / enemy.stats(6)
        case "slash" =>  out.println("You have done " + damagedelt * yous(2) / enemy.stats(7) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * yous(2) / enemy.stats(7)
        case "crush" =>  out.println("You have done " + damagedelt * yous(3) / enemy.stats(8) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * yous(3) / enemy.stats(8)
        case "magic" =>   out.println("You have done " + damagedelt * yous(4) / enemy.stats(9) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * yous(4) / enemy.stats(9)
        case "ranged" =>  out.println("You have done " + damagedelt * yous(5) / enemy.stats(10) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * yous(5) / enemy.stats(10)
      }
      yous(0) = yous(0) - damagetaken * fow.stats(fow.attack) / yous(6)
      out.println(" You have taken " + damagetaken * enemy.stats(1) / yous(6) + " points of damage.")
    }
    if(yous(0) <= 0) out.println("You lost try again.")
    else if(fow.stats(0) <= 0) {
      out.println("You won. here is your reward!")
      //val drop = lootDrop(enemy.drops)
      //val dropName = (for(i <- 0 to drop.length-1) yield {drop(i).name}).toList
      //if(drop.length == 0) out.println("Bad luck no drop")
      //else out.println(dropName.mkString(", "))
      //for(i <- drop) addToInventory(i)
      //fow.stats(0) = reset
    }
  }
  
  
  
  def tItems(): Array[Item] = {
    kit = false
    Array(
    Item("marker","Its red and used to grade tests!",1,Array(0, 100, 100, 100, 100, 100, 0, 0, 0, 0, 0),
        true,"hand"),
    Item("ponytail","It is a symbol of power!",1,Array(25, 0, 0, 0, 0, 0, 25, 25, 25, 25, 25),
        true,"head"),
    Item("t-shirt","It says 'YOU SHAL NOT PASS!'",1,Array(25, 0, 0, 0, 0, 0, 25, 25, 25, 25, 25),
        true,"top"),
    Item("slacks","They give off an aura of misfortune...",1,Array(25, 0, 0, 0, 0, 0, 25, 25, 25, 25, 25),
        true,"hand"),
    Item("roller_skates","They strike fear into the hearts of all who gaze apon them!",
        1,Array(25, 0, 0, 0, 0, 0, 25, 25, 25, 25, 25),true,"hand"))
  }
  def sItems(): Array[Item] = {
    kit = false
    Array(
    Item("axe","It is used to cut down trees. It is not the best but it gets the job done.",1,Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        true,"hand"),
    Item("pickaxe","It is used to mine.",1,Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        true,"hand"),
    Item("leather_top","chest plates are good but you poor af.",1,Array(0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2),
        true,"top"),
    Item("leather_bottom","hahaha they are just pants.",1,Array(0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2),
        true,"bottom"),
    Item("boots","Boots and cats and boots and cats.",
        1,Array(0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2),true,"shoes"))
  }
}