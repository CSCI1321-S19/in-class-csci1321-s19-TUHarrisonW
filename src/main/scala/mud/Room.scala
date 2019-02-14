package MUD

class Room (
    name: String,
    desc: String,
    private var items: List[Item],
    exits: Array[String],
    enemies: Array[Enemy]){
  
  def description(): String = { "\n" + name + "\n" + desc + "\n Exits: " + 
    (for(i <- 0 to exits.length-1) yield {if(getExit(i) == None) "" else direction(i)}).filter(_ != "").mkString(", ") + 
    "\n Items: " + (for(i <- 0 to items.length-1) yield {if(items(i).qty == 1) items(i).name else 
        items(i).name + "(" + items(i).qty + "X)"}).toList.mkString(", ") + "\nEnemies: " + 
        (for(i <- 0 to enemies.length-1) yield { enemies(i).name }).mkString(", ")
   
  }
    
  def getExit(dir: Int): Option[Room] = {
    if(exits(dir) == "-1") None else Room.rooms.get("L_Castle")
  }
  
  def getItem(itemName: String, itemAmount: Int = -1): Item = {
     val inames = for(i <- 0 to items.length-1) yield {items(i).name}
     val check = inames.indexOf(itemName)
     println("The item: " + items(check).name + " has been added to your inventory!")
     if(itemAmount != -1){
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
  
  def enemyNamesperRoom(): List[String] = {
    (for(i <- 0 to enemies.length-1) yield {enemies(i).name}).toList
  }
  
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
  
  def enemy(enemyName: String): Enemy = {
    val elist = (for(i <- 0 to enemies.length-1) yield {enemies(i).name}).toList
    val check = elist.indexOf(enemyName)
    enemies(check)
  }
}
  
object Room {
  val rooms = readRooms()
  
  def readRooms(): Map[String, Room] = {
    val source = scala.io.Source.fromFile("Map.txt")
    val lines = source.getLines()
    val rooms = Array.fill(lines.next.trim.toInt)(readRoomroom(lines))
    source.close()
    rooms.toMap
  }
  
  def readRoomroom(lines: Iterator[String]): (String, Room) = {
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
    val space = lines.next
    (keyWord, new Room(name, desc, items, exits, enemy))
  }
}