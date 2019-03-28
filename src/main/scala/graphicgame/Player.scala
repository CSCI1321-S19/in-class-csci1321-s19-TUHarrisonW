package graphicgame

class Player(xp: Double, yp: Double, wp: Double, hp: Double) extends Entity{  
  private var upHeld = false
  private var leftHeld = false
  private var rightHeld = false
  private var downHeld = false
  private var _stillHere = true
  private var spaceHeld = false
  private var _bnumb = 0 
  def uHeld = upHeld
  def lHeld = leftHeld
  def rHeld = rightHeld
  def dHeld = downHeld
  def sHeld = spaceHeld
  
  def height: Double = hp
  def width: Double = wp
  def x: Double = xp
  def y: Double = yp
  def Update(delay: Double): Unit = ???
	def postCheck():Unit = ???
	def kill(): Unit = _stillHere = false
	def stillHere(): Boolean = _stillHere
	def bnumb(): Int = _bnumb
	def bombadd(): Unit = _bnumb = 1
  def bombkill(): Unit = _bnumb = 0

	//Key Movements
	def move(dx: Double,dy: Double): Player = new Player(x + dx,y + dy, 1.0,1.0)
  
	def upPressed() = upHeld = true
  def leftPressed() = { 
    leftHeld = true
  }
  def rightPressed() = rightHeld = true
  def downPressed() = downHeld = true
  def spacePressed() = spaceHeld = true
  def upReleased() = upHeld = false
  def leftReleased() = leftHeld = false
  def rightReleased() = rightHeld = false
  def downReleased() = downHeld = false
	def spaceReleased() = spaceHeld = false


  
}