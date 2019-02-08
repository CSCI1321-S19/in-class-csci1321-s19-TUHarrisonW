package graphicgame

class Enemy(xp: Double, yp: Double, wp: Double, hp: Double) extends Entity {
  def height: Double = hp
  def width: Double = wp
  def x: Double = xp
  def y: Double = yp
  def Update(delay: Double): Unit = ???
	def postCheck():Unit = ???
	def stillHere(): Boolean = ??? 
	
	def move(dx: Int,dy: Int): Enemy = new Enemy(x + dx,y + dy,0,0)

}