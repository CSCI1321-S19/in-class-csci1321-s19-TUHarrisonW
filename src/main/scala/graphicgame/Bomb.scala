package graphicgame

class Bomb(xp: Double, yp: Double, wp: Double, hp: Double) extends Entity {
  private var _stillHere = true
  
  def height: Double = hp
  def width: Double = wp
  def x: Double = xp
  def y: Double = yp
  def style: Int = 3
  def score: Int = 0
  def spawn(x: Double, y: Double): Bomb = new Bomb(x,y,1.0,1.0)
  def kill(): Unit = _stillHere = false
  def stillHere(): Boolean = _stillHere
}

/*if (sHeld) {
        if (bnumb == 0){
          bombadd()
          new Bomb(x, y, 1.0, 1.0)
        } else player
      } else player
    }
*/