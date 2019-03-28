package mud

import java.net.Socket
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.io.StdIn._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object NetworkTest extends App{
  val sock = new Socket("localhost",4311)
  val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
  val out = new PrintStream(sock.getOutputStream)
  var stopped = false
  Future {
    while(!stopped) {
      val p = in.readLine()
      println(p)
      if(p == "That name is already taken or contains a space. Reconnect and try another name.") stopped = true
    }
  }
  var input = ""
  while(input != "exit") {
    val input = readLine
    out.println(input)
  }
  sock.close()
  stopped = true
}