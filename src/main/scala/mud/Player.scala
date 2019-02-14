package MUD

import collection.mutable.Buffer

class Player extends Combat{
  private val combat = new Combat
  private var croom = "L_Castle" 
  private var inventory = Buffer[Item]()
  private var inventoryNumber = (for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
      else inventory(i).qty}).sum
  private var equipment = Buffer[Item]()
  //Player Stats key: 
  //  1) Hp
  //  2-5) Attack: Stab, Slash, Crush, Magic, Ranged
  //  6-10 Defence: Stab, Slash, Crush, Magic, Ranged
  private var playerStats = Buffer[Int](15,1,1,1,1,1,1,1,1,1,1)
  private var kit = true
      
  def processCommand(command: String): Unit = {
    val test = if(command.indexOf(" ") != -1) (command.split(" ").map(_.trim)).toList else List(command, "none")
    test(0) match {
        case "north" => move(0)
        case "south" => move(1)
        case "east" => move(2)
        case "west" => move(3)
        case "up" => move(4)
        case "down" => move(5)
        case "n" => move(0)
        case "s" => move(1)
        case "e" => move(2)
        case "w" => move(3)
        case "u" => move(4)
        case "d" => move(5)
        case "get" => if(inventoryNumber >= 28) println("Your inventory is full.") 
            else if(Room.rooms(croom).itemNamesperRoom().indexOf(test(1)) == -1) println("That item is not in the room.")
            else if(test.length -1 == 2) addToInventory(Room.rooms(croom).getItem(test(1),test(2).toInt))
            else
                addToInventory(Room.rooms(croom).getItem(test(1)))
        case "drop" => if(itemListInventory().indexOf(test(1)) == -1) println("That item is not in your inventory.")
            else if(test.length -1 == 2) Room.rooms(croom).dropItem(getFromInventory(test(1), test(2).toInt), test(2).toInt)
            else Room.rooms(croom).dropItem(getFromInventory(test(1)))
        case "look" => println(Room.rooms(croom).description())
        case "inv" => println(inventoryListing)
        case "inventory" => println(inventoryListing)
        case "help" => help()
        case "inspect" => inspectItem(test(1))
        case "equip" => if(itemListInventory().indexOf(test(1)) == -1) println("That item is not in your inventory.")
            else equipItem(getFromInventory(test(1), 1))
        case "eview" => println(equipListing())
        case "unequip" => if(itemListEquipment().indexOf(test(1)) == -1) println("That item is not equiped.")
            else addToInventory(unequipItem(test(1)))
        case "stats" => println(statView())
        case "kit" => if(kit) for(i <- tItems) equipItem(i) else
          println("That is not a command. Type Help for a list of valid commands.")
        case "attack" => if(Room.rooms(croom).enemyNamesperRoom.indexOf(test(1)) == -1) println("That enemy is not in the room.")
            else combat(Room.rooms(croom).enemy(test(1)))
        case "enemy_stats" => if(Room.rooms(croom).enemyNamesperRoom.indexOf(test(1)) == -1) println("That enemy is not in the room.")
             else println(Room.rooms(croom).enemy(test(1)).stats.mkString(", "))
        case _ => println("That is not a command. Type Help for a list of valid commands.")
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
    println((for(i <- 0 to inventory.length-1) yield {if(inventory(i).name == "gold") 1
      else inventory(i).qty}).sum + "/28")
    if(inventory.length == 0) "You have no items in your inventory." else 
      (for(i <- 0 to inventory.length-1) yield {inventory(i).name + "(" + 
        inventory(i).qty + "X) - " + inventory(i).desc}).toList.mkString("\n")
  }
  
  def itemListInventory(): List[String] = {
    (for(i <- 0 to inventory.length-1) yield {inventory(i).name}).toList
  }
  
  def itemListEquipment(): List[String] = {
    (for(i <- 0 to equipment.length-1) yield {equipment(i).name}).toList
  }
  
  def move(dir: Int): Unit = {
     if(Room.rooms(croom).exit()(dir) == "-1") println("You can not go that way") else {
       croom = Room.rooms(croom).exit()(dir)
       println(Room.rooms(croom).description())
       kit = false
     }
     
  }
  def help(): Unit = {
    println("""north, south, east, west, up, down - For movement (it is nice if you allow single letter abbreviations)
look - Reprints the description of the current room
inv/inventory - List the contents of your inventory
get *item *amount - To get an item from the room and add it to your inventory. Default for amount is all.
drop *item *amount - To drop an item from your inventory into the room. Default for amount is all.
exit - Leave the game
help - Print the available commands and what they do
inspect *item - Looks at item's information that is located inside your inventory.
attack *enemy - Enter combat with enemy.
eview - Looks at the items currently equiped.
equip *item - Adds item to players active equipment.
unequip *item - Removes item from players active equipment.
stats - Looks at player stats.
enemy_stats - Looks at an enemie's stats.
""")

  }
  
  def inspectItem(itemName: String): Unit = {
    val ilnames = itemListInventory()
    val ienames = itemListEquipment()
    if(ilnames.indexOf(itemName) == -1 && ienames.indexOf(itemName) == -1) 
      println("You do not have that item inside your inventory or equiped.") 
    else if(itemName == "gold") {
      val check = ilnames.indexOf(itemName)
      val done = inventory(check)
      println("Name: " + done.name + "\nQuantity: " + done.qty + 
          "\nDescription: " + done.desc)
    } else {
      if(ilnames.indexOf(itemName) != -1) {
        val check = ilnames.indexOf(itemName)
        val done = inventory(check)
        println("Name: " + done.name + "\nStats: " + done.stats.mkString(", ") + "\nQuantity: " + done.qty + 
          "\nDescription: " + done.desc  + "\nEquipable: " + done.equipable + (if(done.equipable)
            "\nSlot: " + done.slot).toString)
      } else {
        val check = ienames.indexOf(itemName)
        val done = equipment(check)
        println("Name: " + done.name + "\nStats: " + done.stats.mkString(", ") + "\nQuantity: " + done.qty + 
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
    val islot = itemListEquipment()
    if(item.equipable && islot.indexOf(item.slot) == -1) {
        equipment += item
        statsUpdate(item.stats)
        println("The item has been equiped")
    } else { 
      println("You already have an item equiped in that slot.")
      addToInventory(item)
    }
  }
  
  def unequipItem(itemName: String): Item = {
    val iname = itemListEquipment()
    println(iname)
    val check = iname.indexOf(itemName)
    val done = equipment(check)
    statsUpdate(done.stats.map(_ * -1))
    equipment -= equipment(check)
    done
  }
  
  def statsUpdate(stats: Array[Int]): Unit = {
    val pls = playerStats
    val its = stats
    playerStats = (for(i <- 0 to 10) yield {pls(i)+its(i)}).toBuffer
  }
  
  def statView(): String = {
    val ps = playerStats
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
    var you = playerStats
    println("You have entered combat with a " + enemy.name + "!")
    while(fow.stats(0) >= 0 && you(0) >= 0) {
      println("Your HP: " + you(0) + "			" + enemy.name + " HP: " + fow.stats(0))
      println("What attack would you like to use? \n")
      println("Stab, Slash, Crush, Magic, or Ranged?")
      val attack = readLine.toLowerCase().trim()
      val damagedelt = damage
      val damagetaken = damage
      attack match {
        case "stab" => println("You have done " + damagedelt * you(1) / enemy.stats(6) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * you(1) / enemy.stats(6)
        case "slash" =>  println("You have done " + damagedelt * you(2) / enemy.stats(7) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * you(2) / enemy.stats(7)
        case "crush" =>  println("You have done " + damagedelt * you(3) / enemy.stats(8) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * you(3) / enemy.stats(8)
        case "magic" =>   println("You have done " + damagedelt * you(4) / enemy.stats(9) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * you(4) / enemy.stats(9)
        case "ranged" =>  println("You have done " + damagedelt * you(5) / enemy.stats(10) + " damage.")
          fow.stats(0) = fow.stats(0) - damagedelt * you(5) / enemy.stats(10)
      }
      you(0) = you(0) - damagetaken * fow.stats(fow.attack) / you(6)
      println(" You have taken " + damagetaken * enemy.stats(1) / you(6) + " points of damage.")
    }
    if(you(0) <= 0) println("You lost try again.")
    else if(fow.stats(0) <= 0) {
      println("You won. here is your reward!")
      val drop = lootDrop(enemy.drops)
      val dropName = (for(i <- 0 to drop.length-1) yield {drop(i).name}).toList
      if(drop.length == 0) println("Bad luck no drop")
      else println(dropName.mkString(", "))
      for(i <- drop) addToInventory(i)
      fow.stats(0) = reset
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
        1,Array(10, 0, 0, 0, 0, 0, 25, 25, 25, 25, 25),true,"hand"))
  }
}