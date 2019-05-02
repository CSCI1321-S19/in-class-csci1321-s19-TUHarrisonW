package graphicgame

import collection.mutable

class Boss(xp: Double, yp: Double, wp: Double, hp: Double, var plr:Int) extends ArrayQueue with Entity {
  private var _stillHere = true
  
  def height: Double = hp
  def width: Double = wp
  def x: Double = xp
  def y: Double = yp
  def style: Int = 4
  def score: Int = 0
  def Update(delay: Double): Unit = ???
	def postCheck():(Double,Double) = (xp,yp)
	def kill(): Unit = _stillHere = false
	def respawn():Boss = {
    new Boss(56,56,1.0,1.0, plr)
  }
	def stillHere(): Boolean = _stillHere
	def who(): Int = plr
	def changewho(nplr: Int): Unit = {plr = nplr}

	
	def move(dx: Double,dy: Double): Boss = { 
    new Boss(x + dx,y + dy,1,1, plr)
  }
  
  val offsets = List((-1,0), (1,0), (0,1), (0,-1))
  
  def shortMove(boss: Boss, i: Int, maze: Maze, player: Player): Boss = {
    val theWay = for(i <- boss.offsets) yield {
      boss.shortestPath(boss.x.toInt + i._1, boss.y.toInt + i._2, player.x.toInt, player.y.toInt, maze)
    }
    val minWay = theWay.indexOf(theWay.min)
    val (x,y) = offsets(minWay)
    move(x,y)
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