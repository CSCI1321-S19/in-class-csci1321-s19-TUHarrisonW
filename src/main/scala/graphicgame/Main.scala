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
      for (r <- -5 until level.maze.height + 5) {
        for (c <- -5 until level.maze.width + 5) {
          if (level.maze(r, c) == Wall) print('#') else print(' ')
        }
        println()
      }
		  
		  content = canvas
		  
		  onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Up => level.player.upPressed()
          case KeyCode.Down => level.player.downPressed()
          case KeyCode.Left => level.player.leftPressed()
          case KeyCode.Right => level.player.rightPressed()
          case _ =>
        }
      }
      
      onKeyReleased = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Up => level.player.upReleased()
          case KeyCode.Down => level.player.downReleased()
          case KeyCode.Left => level.player.leftReleased()
          case KeyCode.Right => level.player.rightReleased()
          case _ =>
        }
      }
		  
		  
		  var lastTime = -1L
      val timer = AnimationTimer(time => {
        if (lastTime != -1) {
          val delay = (time - lastTime) / 1e9
          val xed = level.player.x
          level.update(delay)
          renderer.render(level, 50, 40)
        }
        lastTime = time
      })
      timer.start()
		}
	}
}