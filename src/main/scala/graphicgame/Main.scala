package graphicgame

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.image.ImageView
import scalafx.scene.image.Image
import scalafx.scene.canvas.Canvas
import scalafx.animation.AnimationTimer
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color

/**
 * This is a stub for the graphical game.
 */
object Main extends JFXApp {
	stage = new JFXApp.PrimaryStage {
		title = "Hit and Run" // Change this to match the theme of your game.
		scene = new Scene(1000, 800) {
		  val canvas = new Canvas(1000, 800)
      val gc = canvas.graphicsContext2D
      val renderer = new Renderer2D(gc,10)
      val level = new Level
      val sock = "jack"
      val sock2 = "jack2"
      level.addPlr(sock)
      level.addPlr(sock2)
      //for (r <- -5 until level.maze.height + 5) {
      //  for (c <- -5 until level.maze.width + 5) {
      //    if (level.maze(r, c) == Wall) print('#') else print(' ')
      //  }
      //  println()
      //}
		  
		  content = canvas
		  
		  onKeyPressed = (ke: KeyEvent) => {
		    if(level.lstplayer.length != 0){
        ke.code match {
          case KeyCode.Up => level.lstplayer(level.lstplayerOrder.indexOf(sock)).upPressed()
          case KeyCode.Down => level.lstplayer(level.lstplayerOrder.indexOf(sock)).downPressed()
          case KeyCode.Left => level.lstplayer(level.lstplayerOrder.indexOf(sock)).leftPressed()
          case KeyCode.Right => level.lstplayer(level.lstplayerOrder.indexOf(sock)).rightPressed()
          case KeyCode.Space => level.lstplayer(level.lstplayerOrder.indexOf(sock)).spacePressed()
          case KeyCode.W => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).upPressed()
          case KeyCode.S => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).downPressed()
          case KeyCode.A => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).leftPressed()
          case KeyCode.D => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).rightPressed()
          case KeyCode.Z => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).spacePressed()
          case _ =>
        }
		    }
      }
      
      onKeyReleased = (ke: KeyEvent) => {
        if(level.lstplayer.length != 0){
        ke.code match {
          case KeyCode.Up => level.lstplayer(level.lstplayerOrder.indexOf(sock)).upReleased()
          case KeyCode.Down => level.lstplayer(level.lstplayerOrder.indexOf(sock)).downReleased()
          case KeyCode.Left => level.lstplayer(level.lstplayerOrder.indexOf(sock)).leftReleased()
          case KeyCode.Right => level.lstplayer(level.lstplayerOrder.indexOf(sock)).rightReleased()
          case KeyCode.Space => level.lstplayer(level.lstplayerOrder.indexOf(sock)).spaceReleased()
          case KeyCode.W => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).upReleased()
          case KeyCode.S => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).downReleased()
          case KeyCode.A => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).leftReleased()
          case KeyCode.D => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).rightReleased()
          case KeyCode.Z => level.lstplayer(level.lstplayerOrder.indexOf(sock2)).spaceReleased()
          case _ =>
        }
        }
      }
		  
		  
		  var lastTime = -1L
      val timer = AnimationTimer(time => {
        if (lastTime != -1) {
          val delay = (time - lastTime) / 1e9
          if(level.lstplayer.length != 0){
            level.update(delay, sock)
            level.update(delay, sock2)
            val pl = level.makePassable()
            renderer.render(pl)
        }
        }
        lastTime = time
      })
      timer.start()
		}
	}
}