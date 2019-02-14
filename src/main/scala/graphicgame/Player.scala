package graphicgame

class Player(xp: Double, yp: Double, wp: Double, hp: Double) extends Entity{  
  private var upHeld = false
  private var leftHeld = false
  private var rightHeld = false
  private var downHeld = false
  def uHeld = upHeld
  def lHeld = leftHeld
  def rHeld = rightHeld
  def dHeld = downHeld
  
  def height: Double = hp
  def width: Double = wp
  def x: Double = xp
  def y: Double = yp
  def Update(delay: Double): Unit = ???
	def postCheck():Unit = ???
	def stillHere(): Boolean = ??? 

	//Key Movements
	def move(dx: Double,dy: Double): Player = new Player(x + dx,y + dy, 1.0,1.0)
  
	def upPressed() = upHeld = true
  def leftPressed() = { 
    leftHeld = true
  }
  def rightPressed() = rightHeld = true
  def downPressed() = downHeld = true
  def upReleased() = upHeld = false
  def leftReleased() = leftHeld = false
  def rightReleased() = rightHeld = false
  def downReleased() = downHeld = false
	def fireUpPressed(): Unit = ???
	def fireDownPressed(): Unit = ???
	def fireLeftPressed(): Unit = ???
	def fireRightPressed(): Unit = ???
	def fireUpReleased (): Unit = ???
	def fireDownReleased (): Unit = ???
	def fireLeftReleased (): Unit = ???
	def fireRightReleased (): Unit = ???

  
}