package mud

import io.StdIn._
import akka.actor.Props
import akka.actor.Actor
import scala.concurrent.duration._
import java.net.ServerSocket
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import akka.actor.ActorRef

object Main {
  def main(args: Array[String]): Unit = {
    val playerManager = start._playerManager
    val roomManager = start._roomManager
    val npcManager = start._npcManager
    val system = start.system
    
    //roomManager ! cases.readRooms(roomManager)
    //npcManager ! cases.readNPCs(npcManager)
    Startup
    println("Ready")
    
    
    var option = ""
    
    implicit val ec = system.dispatcher
    system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, cases.CheckAllInput)
    
      val ss = new ServerSocket(4311)
      while(true) {
        val sock = ss.accept()
        val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
        val out = new PrintStream(sock.getOutputStream)
        Future {
          out.println("What is your name? (No spaces)")
          val name = in.readLine().toLowerCase()
          playerManager ! cases.CreateChild(sock, in, out, name)
          playerManager ! cases.directCmd(name, "look")
        }
      }
  }
}