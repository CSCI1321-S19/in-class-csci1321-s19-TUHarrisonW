package graphicgame

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.net.Socket
import scalafx.scene.input.KeyCode

class Player(xp: Double, yp: Double, wp: Double, hp: Double, _score: Int) extends Entity{  
  private var upHeld = false
  private var leftHeld = false
  private var rightHeld = false
  private var downHeld = false
  private var _stillHere = true
  private var spaceHeld = false
  private var _bnumb = 0
  private var scores = _score
  def uHeld = upHeld
  def lHeld = leftHeld
  def rHeld = rightHeld
  def dHeld = downHeld
  def sHeld = spaceHeld
  
  def height: Double = hp
  def width: Double = wp
  def x: Double = xp
  def y: Double = yp
  def style: Int = 1
  def score: Int = scores
  def Update(delay: Double): Unit = ???
	def postCheck():Unit = ???
	def kill(): Unit = _stillHere = false
	def stillHere(): Boolean = _stillHere
	def bnumb(): Int = _bnumb
	def bombadd(): Unit = _bnumb = 1
  def bombkill(): Unit = _bnumb = 0

  def addscore():Unit = scores += 1
	//Key Movements
	def move(dx: Double,dy: Double): Player = new Player(x + dx,y + dy, 1.0,1.0, scores)
  
	def upPressed() = {
    upHeld = true
  }
  def leftPressed() = { 
    leftHeld = true
  }
  def rightPressed() = {
    rightHeld = true
  }
  def downPressed() = {
    downHeld = true
  }
  def spacePressed() = spaceHeld = true
  def upReleased() = {
    upHeld = false
  }
  def leftReleased() = leftHeld = false
  def rightReleased() = rightHeld = false
  def downReleased() = downHeld = false
	def spaceReleased() = spaceHeld = false
	
	def placeBomb(): Bomb = {
          bombadd()
          new Bomb(x,y,1.0,1.9)
  }
	
	def playerMove(player:Player, maze:Maze): Player = {
	  if (lHeld){
      if(Entity.isClear(-1,0,player, maze)){
        move(-1, 0)
      }else player
	  }else if (uHeld){
      if(Entity.isClear(0, -1, player, maze)){
        move(0, -1)
      } else player
	  }else if (dHeld){
      if(Entity.isClear(0, 1, player, maze)){
        move(0, 1)
      }else player
	  }else if (rHeld){
      if(Entity.isClear(1, 0, player, maze)){
        move(1, 0) 
      }else player
    }else player
  }
  
}