package MUD
import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    val player = new Player
    val room = Room.rooms
    var option = ""
    var x = true
    println(room(0).description())
    while(x == true) {
      option = readLine.toLowerCase().trim()
      if(option == "exit") x = false else player.processCommand(option)
    }
  }
}