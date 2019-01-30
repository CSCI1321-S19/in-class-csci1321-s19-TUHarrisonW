package MUD
import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    val player = new Player
    val room = Room.rooms
    var option = ""
    var x = 1
    println(room(player.croom).description())
    while(x > 0) {
      option = readLine.toLowerCase().trim()
      if(option == "exit") x = 0 else player.processCommand(option)
    }
  }
}