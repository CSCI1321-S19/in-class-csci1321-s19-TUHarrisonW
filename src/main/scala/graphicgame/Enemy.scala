package graphicgame

import collection.mutable

class Enemy(xp: Double, yp: Double, wp: Double, hp: Double) extends ArrayQueue with Entity {
  private var _stillHere = true
  
  def height: Double = hp
  def width: Double = wp
  def x: Double = xp
  def y: Double = yp
  def Update(delay: Double): Unit = ???
	def postCheck():Unit = ???
	def kill(): Unit = _stillHere = false
	def stillHere(): Boolean = _stillHere

	
	def move(dx: Double,dy: Double): Enemy = { 
    new Enemy(x + dx,y + dy,1,1)
  }
  
  val offsets = List((-1,0), (1,0), (0,1), (0,-1))
  
  def shortMove(enemy: Enemy, i: Int, maze: Maze, player: Player): Enemy = {
    val theWay = for(i <- enemy.offsets) yield {
      enemy.shortestPath(enemy.x.toInt + i._1, enemy.y.toInt + i._2, player.x.toInt, player.y.toInt, maze)
    }
    val minWay = theWay.indexOf(theWay.min)
    val (x,y) = offsets(minWay)
      if(Entity.isClear(x,y,enemy, maze)) { 
        move(x,y)
      }
      else {
        val minWay = theWay.indexOf(theWay.min,theWay.indexOf(theWay.min) + 1)
        if(minWay != -1) {
          val (x,y) = offsets(minWay)
          if(Entity.isClear(x,y,enemy, maze)) move(x,y)
          else enemy
        } else enemy
      }
  }
  
  def shortestPath(sx: Int, sy: Int, ex: Int, ey: Int, maze: Maze): Int = {
    val q = new ArrayQueue[(Int,Int,Int)]()
    q.enqueue(sx,sy,0)
    val visited = mutable.Set[(Int,Int)](sx -> sy)
    while(!q.isEmpty) {
      val (x, y, steps) = q.dequeue()
      for((dx, dy) <- offsets) {
        val nx = x + dx
        val ny = y + dy
        if (nx == ex && ny == ey) { 
          return steps + 1
        }
        if (nx >= 0 && nx < maze.height && ny >= 0 && ny < maze.width && 
            maze(nx,ny) != Wall && !visited(nx -> ny)) {
          q.enqueue(nx, ny, steps + 1)
          visited += nx -> ny
        }
      }
    }
    10000000
  }

}