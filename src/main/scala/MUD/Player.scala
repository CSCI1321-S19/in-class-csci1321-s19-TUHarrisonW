package MUD

class Player {
  private var croom = 0
  private var inventory = List[Item]()
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
        case "get" => if(inventory.length-1 == 27) println("Your inventory is full.") 
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
        case _ => println("That is not a command. Type Help for a list of valid commands.")
    }
  }
  
  def getFromInventory(itemName: String, itemAmount: Int = -1): Item = {
     val inames = for(i <- 0 to inventory.length-1) yield {inventory(i).name}
     val check = inames.indexOf(itemName)
     if(inventory(check).qty > 1) {
       if(itemAmount != -1) {
         inventory = inventory.patch(check, Seq(inventory(check).copy(qty = inventory(check).qty - itemAmount)), 1)
         inventory(check)
       } else {
       val itm = inventory(check)
       inventory = inventory.filter(_.name != itemName)
       itm
       }
     } else { 
       val itm = inventory(check)
       inventory = inventory.filter(_.name != itemName)
       itm
     }
  }
  
  
  
  def addToInventory(item: Item): Unit = {
    println("You have picked up a " + item.name + "!")
    val inames = for(i <- 0 to inventory.length-1) yield {inventory(i).name}
    if(inames.indexOf(item.name) != -1) {
       val check = inames.indexOf(item.name)
       inventory = inventory.patch(check, Seq(inventory(check).copy(qty = inventory(check).qty + item.qty)), 1)
    } else
    inventory = inventory ::: List(item)
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
  
  def move(dir: Int): Unit = {
     if(Room.rooms(croom).exit()(dir) == -1) println("You can not go that way") else {
       croom = Room.rooms(croom).exit()(dir)
       println(Room.rooms(croom).description())
     }
     
  }
  def help(): Unit = {
    println("""north, south, east, west, up, down - for movement (it is nice if you allow single letter abbreviations)
look - reprints the description of the current room
inv/inventory - list the contents of your inventory
get item amount - to get an item from the room and add it to your inventory. Default for amount is all.
drop item amount - to drop an item from your inventory into the room. Default for amount is all.
exit - leave the game
help - print the available commands and what they do
inspect item - looks at item's information that is located inside your inventory.
""")

  }
  
  def inspectItem(itemName: String): Unit = {
    val inames = for(i <- 0 to inventory.length-1) yield {inventory(i).name}
    if(inames.indexOf(itemName) == -1) println("You do not have that item inside your inventory.") else {
      val check = inames.indexOf(itemName)
      val done = inventory(check)
      println("Name: " + done.name + "\nStats: " + done.stats + "\nQuantity: " + done.qty + "\nDescription: " + done.desc)
    }
  }

}