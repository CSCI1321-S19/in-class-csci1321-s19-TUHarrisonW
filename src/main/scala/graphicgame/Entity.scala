package graphicgame

trait Entity extends Serializable {
  def x: Double
  def y: Double
  def width: Double
  def height: Double
  def style: Int
  def score: Int
  
  def makePassable(): PassableEntity = PassableEntity(x, y, width, height, style, score)
  
//  def update(delay: Double): Unit
//  def postCheck(): Unit // You can delete this if you don't use it.
//  def stillHere(): Boolean // This is how you should remove entites from the level.
}

object Entity {
  
  def isClear(x: Int, y: Int, ent: Entity, maze: Maze): Boolean = {
    if(maze(ent.x.toInt + x, ent.y.toInt + y) != Wall) true 
    else false
  }
  def intersect(e1: Entity, e2: Entity): Boolean = {
    ((e1.x-e2.x).abs < (e1.width+e2.width)/2) && ((e1.y-e2.y).abs < (e1.height+e2.height)/2)
  }
}