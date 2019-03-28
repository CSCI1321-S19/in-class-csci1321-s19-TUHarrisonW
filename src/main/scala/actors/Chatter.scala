package actors

import akka.actor.Actor
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket

class Chatter(sock: Socket, in: BufferedReader, out: PrintStream, name: String) extends Actor {
  import Chatter._
  def receive = {
    case CheckInput =>
      if (in.ready()) {
        val input = in.readLine()
        context.parent ! ChatManager.SendMessageToAll(name+" said "+input)
      }
    case m => println("oops in chat manager: " +m) 
  }
}

object Chatter {
  case object CheckInput
  case class PrintMessage(message: String)
}