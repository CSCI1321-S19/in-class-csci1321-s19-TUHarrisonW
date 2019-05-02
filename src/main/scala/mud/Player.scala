package mud

import collection.mutable.Buffer
import akka.actor.Actor
import akka.actor.ActorRef
import cases._
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import scala.io.StdIn._


class Player (sock: Socket, in: BufferedReader, out: PrintStream, name: String, sroom: String) extends Actor{
  private val playerManager = start._playerManager
  private val roomManager = start._roomManager
  private val activityManager = start._activityManager
  private val npcManager = start._npcManager
  private val server = start._server
  private var _roomMap = Map[String, ActorRef]()
  private var croom = sroom
  private var croomNPCs = List[String]()
  private var inventory = Buffer[Item]()
  private var inventoryNumber = (for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
      else inventory(i).qty}).sum
  private var equipment = Buffer[Item]()
  private val _name = name
  private var hold = false
  //Player Stats key: 
  //  1) Hp
  //  2-5) Attack: Stab, Slash, Crush, Magic, Ranged
  //  6-10 Defence: Stab, Slash, Crush, Magic, Ranged
  private var playerStats = Buffer[Int](15,3,3,3,3,3,1,1,1,1,1)
  private var equipStats = Buffer[Int](0,0,0,0,0,0,0,0,0,0,0)
  private var kit = true
  private var sinv = true
  private var npccombat = "none"
  private var hp = playerStats(0) + equipStats(0)
  private var timeout = 0
  private var quiet = false
  private var afk = false
  private var _watch = false
  def hpreset(){
    hp = playerStats(0) + equipStats(0)
    npccombat = "none"
    hold = false
  }
  
  def receive = {
    case s:String => processCommand(s)
    case recievedesc(s:String, name: String) => {
      out.println(s)
      npcManager ! sendplrroom(name, croom)
      playerManager ! sendplrroom(name, croom)
    }
    case checkConnection() => {
      if(name != "admin") {
        if(out.checkError()) {
          if(hold){
            npcManager ! exit(npccombat)
          }
          playerManager ! partyLeave(name)
          playerManager ! exit(name)
          sock.close()
        }
        timeout += 1
      }
    }
    
    case recieveexit(e:Array[String], dir: Int,name: String) => move(e, dir)
    case hit(aEStat:Int, aLStat: Int, dEStat:Int, dLStat:Int, atype:Int, name:String, npcName: String, who:String) => {
      activityManager ! hit(aEStat, aLStat, playerStats(atype+5), playerStats(atype+5), atype, name, npcName, who)
    }
    case hpChange(damage: Int, name:String, npcName:String) => {
      out.println("Damage to you: "+damage)
      hp = hp - damage
      out.println("Your hp: "+hp)
      if(hp <= 0) { 
       activityManager ! combatEnd(name:String, npcName:String, "You where defeated by "+npcName.filter(!_.isDigit)+"! \n"+
                                   "You where rescued by the Lumbridge medic core."+
                                   "\nIn the rush they forgot 1 or more of your items on the battle field.")
       val fullinv = inventory.toList ::: equipment.toList
       var droplocations = Array.fill(util.Random.nextInt(fullinv.length))(util.Random.nextInt(fullinv.length)).distinct
       if(droplocations.length == 0) droplocations = Array.fill(1)(util.Random.nextInt(2))
       for(i <- droplocations) {
         if(inventory.indexOf(fullinv(i)) != -1) {
           roomManager ! sendDropItem(getFromInventory(fullinv(i).name),-1, croom)
         } else {
          roomManager ! sendDropItem(unequipItem(fullinv(i).name),-1, croom)
         }
       }
       croom = "L_Castle"
       hpreset
       playerManager ! moveroom(croom, name)
       npccombat = "none"
      }
    }
    case hpreset(name:String) => {
      hpreset()
    }
    case sendStats(npcStats, name) => out.println(statView(npcStats.toBuffer))
    case recieveItems(items:List[String], test:List[String], name: String) => {
      if (items.indexOf(test(1)) == -1) { 
        
        out.println("That item is not in the room.")
      }
      else if(test.length -1 == 2 && (test(2) forall Character.isDigit))
        roomManager ! sendItem(test(1), test(2).toInt, croom, test, name)
        else 
          roomManager ! sendItem(test(1),-1, croom, test, name)
    }
    case recieveItem(item:Item, test:List[String], name: String) => {
      out.println("The item: " + item.name + " has been added to your inventory!")
      addToInventory(item)
    }
    case npccombatName(pname:String, name:String) => {
      npccombat = name
    }
    case exit(name:String) => playerManager ! exit(name)
    case CheckInput =>{
      if(in.ready()) {
        timeout = 0
        if(afk == true) {
          quiet = false
          afk = false
        }
        val input = in.readLine()
        if(_watch == true) playerManager ! message(input, "admin")
        if(input != "exit") {
         processCommand(input) 
        }
        else {
          playerManager ! partyLeave(name)
          playerManager ! exit(name)
          sock.close()
        }
      }
    }
    case wisperout(s:String) => {
      if(s.indexOf("NPCs: ") == 0 && croomNPCs != s) {
        croomNPCs = s.replace("NPCs: ", "").replace(",", "").split(" ").toList
      }
      out.println(s)
    }
    case chatout(s:String) => {
      if(quiet == false) out.println(s)
    }
    case frees(s:String) => free()
    case watch(name:String) => {
      if(_watch == false) _watch = true
      else _watch = false
    }
    case directCmd(name: String, direction:Int) => startup()
    
    case m => {
      
      out.println("player Unhandled message in Minion: " + m)
    }
  }

  def processCommand(command: String): Unit = {
    val test = if(command.indexOf(" ") != -1) (command.split(" ").map(_.trim)).toList else List(command, "none")
    if(_name == "admin") ServerHost(test)
    else {
      if(hold != true && afk != true) {
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
          } else { 
            roomManager ! sendItems(test, croom, name)
          }
          case "drop" => {
            val inames = itemListInventory()
            val check = inames.indexOf(test(1))
            
            if(itemListInventory().indexOf(test(1)) == -1) {
            
            out.println("That item is not in your inventory.")
          }
              else if(test.length-1 == 2 && (test(2) forall(Character.isDigit)) && inventory(check).qty > test(2).toInt) {
                roomManager ! sendDropItem(getFromInventory(test(1), test(2).toInt), test(2).toInt, croom)
              }
              else roomManager ! sendDropItem(getFromInventory(test(1), -1), -1, croom)
          }
          case "look" => { 
              roomManager ! senddesc(croom, name)
            }
          
          case "tests" => out.println(playerStats.length)
          case "inv" => {
            out.println(inventoryListing)
          }
          case "inventory" =>{
            out.println(inventoryListing)
          }
          case "help" => help()
          case "legend_help" => legendHelp()
          case "party_help" => partyHelp()
          case "combat_help" => combatHelp()
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
          case "stats" => {
            out.println(statView(equipStats))
          }
          case "level" => {
            out.println(statView(playerStats))
          }
          case "lewis" => if(kit) for(i <- tItems) equipItem(i) else {
            out.println(kit)
            out.println("That is not a command. Type Help for a list of valid commands.")
          }
          case "kill" => {
            if(croomNPCs.indexOf(test(1)) == -1) {
              out.println("That NPC is not in your current room.")
            } else {
              hold = true
              activityManager ! attack(name, test(1))
              combatHelp()
            }
          }
          case "npc_stats" => npcManager ! stats(name, test(1))
          case "exit" => {
            out.println("Th-Th-The, Th-Th-The, Th-Th... That's all, folks!")
          }
          case "say" => {
            if (test.length-1 > 1)
              playerManager ! say(name,(for(i <- test if i != test(0)) yield (i)).toList.mkString(" "), croom)
             else playerManager ! say(name, name+" said: "+test(1), croom)
          }
          case "whisper" => {
            playerManager ! wisper(name, (for(i <- test if i != test(0) && i != test(1)) yield (i)).toList.mkString(" "), test(1))
          }
            case "see" => {
              playerManager ! sendplrroom(name,croom)
            }
            case "quiet" => {
              if(quiet == false) {
                quiet = true
                out.println("You have entered quiet mode. Type quiet again to leave quiet mode.")
              } else {
                quiet = false
                out.println("You have left quiet mode. Type quiet again to enter quiet mode.")
              }
            }
            case "party_create" => {
              if(test.length == 1) out.println("Check to make sure the command is formatted correctly.")
              else if(test.length == 2) playerManager ! createParty(name, test(1), "")
              else playerManager ! createParty(name, test(1), test(2))
            }
            case "party_leave" => {
              playerManager ! partyLeave(name)
            }
            case "psay" => {
              playerManager ! psay(name,(for(i <- test if i != test(0)) yield (i)).toList.mkString(" "))
            }
            case "party" => playerManager ! party(name)
            case "spath" => {
              roomManager ! shortestPath(croom, name, test(1), List[String]())
            }
            case "rooms" => roomManager ! sendie(name)
            
          case _ => {
            
            out.println("That is not a command. Type Help for a list of valid commands. " + test(0))
          }
        }
      } else if(hold == true && afk != true){
        test(0) match {
          case "quiet" => {
              if(quiet == false) {
                quiet = true
                out.println("You have entered quiet mode. Type quiet again to leave quiet mode.")
              } else {
                quiet = false
                out.println("You have left quiet mode. Type quiet again to enter quiet mode.")
              }
            }
          case "run" => {
            //out.println("You have escaped!")
            roomManager ! sendexit(croom, util.Random.nextInt(6), name)
          }
          case "stab" => {
            npcManager ! hit(equipStats(1),playerStats(1),0,0,1, name, npccombat, "Your")
            npcManager ! hit2(0, 0, 0, 0, 0, name, npccombat)
          }
          case "slash" => {
            npcManager ! hit(equipStats(2),playerStats(2),0,0,2, name, npccombat, "Your")
            npcManager ! hit2(0, 0, 0, 0, 0, name, npccombat)
          }
          case "crush" => {
            npcManager ! hit(equipStats(3),playerStats(3),0,0,3, name, npccombat, "Your")
            npcManager ! hit2(0, 0, 0, 0, 0, name, npccombat)
          }
          case "magic" => {
            npcManager ! hit(equipStats(4),playerStats(4),0,0,4, name, npccombat, "Your")
            npcManager ! hit2(0, 0, 0, 0, 0, name, npccombat)
          }
          case "ranged" => {
            npcManager ! hit(equipStats(5),playerStats(5),0,0,1, name, npccombat, "Your")
            npcManager ! hit2(0, 0, 0, 0, 0, name, npccombat)
          }
          case "help" => combatHelp()
          case "hp" => npcManager ! hpCheck(name, npccombat, hp, 0)
          case _ => {
            out.println("That is not a combat command. Type Help for a list of valid commands. " + test(1))
          }
        }
      } else {
        test(0) match {
          case _ => out.println("AFK mode has been disabled.")
        }
      }
    }
  }
  
  def startup(): Unit = {
    out.println("\nStarting items have been added to your inventory.")
    for(i <- sItems) addToInventory(i)
    out.println("Type help for a list of commands.")
    processCommand("look") 
    
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
    hold = false
  }
  
  def move(exit: Array[String], dir: Int): Unit = {
     if(exit(dir) == "-1") {
       if(hold) { 
         out.println("You failed to escape.")
         npcManager ! hit2(0, 0, 0, 0, 0, name, npccombat)
       } else out.println("You can not go that way")
     } else {
       if(hold){
         activityManager ! combatEnd(name, npccombat, "You have escaped!")
         hold = false
         hpreset
       }
       croom = exit(dir)
       roomManager ! senddesc(croom, name)
       playerManager ! moveroom(croom, name)
       kit = false
     }
  }
  
  def help(): Unit = {
    val help = ("""
help - This page.
legend_help - Displays an legend of what symbols mean.
party_help - Displays information on party commands.
combat_help - Displays information on combat commands. 
north, south, east, west, up, down - For movement (it is nice if you allow single letter abbreviations)
look - Reprints the description of the current room
inv/inventory - List the contents of your inventory
get *item (*amount) - To get an item from the room and add it to your inventory. Default for amount is all.
drop *item (*amount) - To drop an item from your inventory into the room. Default for amount is all.
exit - Leave the game
inspect *item - Looks at item's information that is located inside your inventory.
eview - Looks at the items currently equiped.
equip *item - Adds item to players active equipment.
unequip *item - Removes item from players active equipment.
stats - Looks at player's stats.
level - Looks at a player's level.
npc_stats *npcName - Looks at an enemie's stats.
whisper *player *message - Sends a private massage to a player.
say *message - Say something to all players in your current room.
see - Displays  at all players inside your current room.
quiet - Blocks room notifications from appearing. (enter, exit, and say)
rooms - Displays a list of all the room numbers along with their name.
spath *Room Number(found by using rooms cmd) - Displays how to get to a certain room.""")
  out.println(help)
  } 
  
  def legendHelp(): Unit = {
    val help = ("""First word is the command.
* = Required information.
(*) = Extra information. Not needed for basic command
WIP = Work in progress. This command has not been finished or still has aspects that need to be fixed.""")
    out.println(help)
  }
  
  def combatHelp(): Unit = {
    val help = ("""kill *enemy - Enter combat with enemy.
stab, slash, crush, magic, ranged - attack with the different styles.
run - try to escape.
hp - check to see both your own and the enemies HP
quiet - Blocks room notifications from appearing. (enter, exit, and say)""")
    out.println(help)
  }
  
  def partyHelp(): Unit = {
    val help = ("""party_create *player1 (*player2) - Create a party with up to 2 other players.
    A party can not consist of only one player. If this occurs the party will be disbanded.
party_invite *player1 (*player2) - If the party count is not maxed (#/3) a player can be invited.
party_leave - Leave the current party. 
psay *message - Say something to all players in your current party.
party - Displays who is in your current party.""")
    out.println(help)
  }
  def aHelp(): Unit = {
    val help = ("""NOTICE: The server will not start till you type start. 
stop: Stops the server
ban *name *reason - bans a player for a reason.
watch *name - Displays all inputs done by the player.
rooms - Displays all room names (number -> KeyWord).
players - Displays all players currently playing.
fmove *name *KeyWord - Force moves a player to a location.""")
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
    hp = playerStats(0) + equipStats(0)
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
      aCrush + "\n" + "		"+aMagic + "	" + aRanged + "\n" + "		Defence" + "\n" + 
      dStab + "	" + dSlash + "	" + dCrush + "\n" +  "		"+dMagic + "	" +dRanged + "\n"
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
        true,"bottom"),
    Item("roller_skates","They strike fear into the hearts of all who gaze apon them!",
        1,Array(10, 0, 0, 0, 0, 0, 25, 25, 25, 25, 25),true,"boots"))
  }
  def sItems(): Array[Item] = {
    //kit = false
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
  
  def ServerHost(test:List[String]){
    test(0) match {
      case "help" => aHelp()
      case "ban" => playerManager ! ban(test(1), test(2))
      case "watch" => playerManager ! watch(test(1))
      case "rooms" => roomManager ! sendie(name)
      case "players" => playerManager ! see("")
      case "fmove" => roomManager ! moveroom(test(2), test(1))
      case "start" => server ! startss()
      case "whisper" => {
            playerManager ! wisper(name, (for(i <- test if i != test(0) && i != test(1)) yield (i)).toList.mkString(" "), test(1))
      }
      case "stop" => {
        playerManager ! wisper("Console","The server is being shutdown. Good bye.","all")
        activityManager ! stopss()
      }
      case _ => out.println("Command not recognized.")
    }
  }
}