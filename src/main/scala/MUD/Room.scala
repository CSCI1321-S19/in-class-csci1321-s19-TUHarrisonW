package MUD

class Room (
    name: String,
    desc: String,
    private var items: List[Item],
    exits: Array[Int]) {
  
  def description(): String = { "\n" + name + "\n" + desc + "\n Exits: " + 
    (for(i <- 0 to exits.length-1) yield {if(getExit(i) == None) "" else direction(i)}).filter(_ != "").mkString(", ") + 
    "\n Items: " + (for(i <- 0 to items.length-1) yield {if(items(i).qty == 1) items(i).name else 
        items(i).name + "(" + items(i).qty + "X)"}).toList.mkString(", ")
    
  }
    
  def getExit(dir: Int): Option[Room] = {
    if(exits(dir) == -1) None else Some(Room.rooms(exits(dir)))
  }
  
  def getItem(itemName: String, itemAmount: Int = -1): Item = {
     val inames = for(i <- 0 to items.length-1) yield {items(i).name}
     val check = inames.indexOf(itemName)
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
     else 
    items = items ::: List(item.copy(qty = item.qty))
  }
  
  def itemNamesperRoom(): List[String] = {
    (for(i <- 0 to items.length-1) yield {items(i).name}).toList
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
  def exit():Array[Int] = exits
}
  
object Room {
  val rooms = readRooms()
  
  def readRooms(): Array[Room] = {
    val source = scala.io.Source.fromFile("Map.txt")
    val lines = source.getLines()
    val rooms = Array.fill(lines.next.trim.toInt)(readRoom(lines))
    source.close()
    rooms
  }
  
  def readRoom(lines: Iterator[String]): Room = {
    val name = lines.next
    val desc = lines.next
    val items = List.fill(lines.next.trim.toInt) {
      Item(lines.next, lines.next, lines.next.toInt, lines.next)
    }
    val exits = lines.next.split(",").map(_.trim.toInt)
    new Room(name, desc, items, exits)
  }
}