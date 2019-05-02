package mud

import scala.concurrent.duration._

object Main {
  def main(args: Array[String]): Unit = {
    val playerManager = start._playerManager
    val activityManager = start._activityManager
    val system = start.system
    
    playerManager ! cases.CreateChild(null, Console.in, Console.out, "admin")
    
    var option = ""
    
    implicit val ec = system.dispatcher

    system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, cases.CheckAllInput)
    system.scheduler.schedule(0.seconds, 0.1.seconds, activityManager, cases.CheckRequest())
    //system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, cases.checkConnection())
  }
}