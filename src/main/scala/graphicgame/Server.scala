package graphicgame

import java.net.ServerSocket
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.net.Socket
import java.util.concurrent.LinkedBlockingQueue

case class User(level: Level, sock: Socket, in: ObjectInputStream, out: ObjectOutputStream, name:String)

object Server extends App {
  private var players = List[User]()
  private val playerQueue = new LinkedBlockingQueue[User]

  private val ss = new ServerSocket(4040)
  val level = new Level
  var numb = 0
  Future {
    while (true) {
      val sock = ss.accept()
      val in = new ObjectInputStream(sock.getInputStream)
      val out = new ObjectOutputStream(sock.getOutputStream)
      val player = User(level, sock, in, out, "A"+numb)
      numb += 1
      level.addPlr(player.name)
      playerQueue.put(player)
    }
  }

  var lastTime = -1L
  while (true) {
    while(playerQueue.size() > 0) {
      val player = playerQueue.take()
      Future {
        while (true) {
          player.level.handleKey(player.in.readObject(), player.name)
        }
      }
      players ::= player
    }
    val time = System.nanoTime()
    if (lastTime != -1) {
      val delay = (time - lastTime) / 1e9
      for (player <- players) {
        if(player.level.update(delay, player.name)) {
          val pb = player.level.makePassable()
          player.out.writeObject(pb)
        }
      }
    }
    lastTime = time
  }
}