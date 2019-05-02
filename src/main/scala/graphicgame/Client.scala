package graphicgame

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.animation.AnimationTimer
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode
import java.net.Socket
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import scalafx.scene.control.TextInputDialog
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.application.Platform

object Client extends JFXApp {
  val boardWidth = 1000
  val boardHeight = 800

  val textDialog = new TextInputDialog("localhost")
  val machine = textDialog.showAndWait().getOrElse("localhost")
  val sock = new Socket(machine, 4040)
  val out = new ObjectOutputStream(sock.getOutputStream)
  val in = new ObjectInputStream(sock.getInputStream)

  val canvas = new Canvas(boardWidth, boardHeight)
  val gc = canvas.graphicsContext2D
  val renderer = new Renderer2D(gc,10)

  stage = new JFXApp.PrimaryStage {
    title = "Dr. Mario"
    scene = new Scene(boardWidth, boardHeight) {
      content = canvas

      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Up => out.writeObject(UpPressed)
          case KeyCode.Down => out.writeObject(DownPressed)
          case KeyCode.Left => out.writeObject(LeftPressed)
          case KeyCode.Right => out.writeObject(RightPressed)
          case KeyCode.Space => out.writeObject(SpacePressed)
          case _ =>
        }
      }
      onKeyReleased = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Up => out.writeObject(UpReleased)
          case KeyCode.Down => out.writeObject(DownReleased)
          case KeyCode.Left => out.writeObject(LeftReleased)
          case KeyCode.Right => out.writeObject(RightReleased)
          case KeyCode.Space => out.writeObject(SpaceReleased)
          case _ =>
        }
      }

      Future {
        while (true) {
          in.readObject() match {
            case pb: PassableLevel =>
              Platform.runLater(renderer.render(pb))
          }
        }
      }
    }
  }

}