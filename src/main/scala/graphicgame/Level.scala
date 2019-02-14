package graphicgame

class Level {
  private var moveDelay = 0.0
  private val moveInterval = 0.0
  private var _player = new Player(1.0, 1.0, 1.0, 1.0)
  private var _enemy = new Enemy(59,59,1.0,1.0)
  val maze = RandomMaze(3, false, 20, 20, 0.6)
  private var ents = List(_player, _enemy)

  def player = _player
  def enemy = _enemy
  
  def entities: List[Entity] = ents
  def update(delay: Double): Unit = {
    moveDelay += delay
    if (moveDelay >= moveInterval ) { 
      if (player.lHeld)
        if(maze(player.x.toInt -1, player.y.toInt) != Wall) _player = player.move(-1, 0)
      if (player.rHeld)
        if(maze(player.x.toInt +1, player.y.toInt) != Wall) _player = player.move(1, 0) 
      if (player.uHeld)
        if(maze(player.x.toInt, player.y.toInt - 1) != Wall) _player = player.move(0, -1)
      if (player.dHeld)
        if(maze(player.x.toInt, player.y.toInt + 1) != Wall) _player = player.move(0, 1)
      if(enemy.x == 59 && enemy.y >= 1 ) _enemy = enemy.move(0, -1)
      if(enemy.y == 1 && enemy.x >= 1) _enemy = enemy.move(-1, 0)
      if(enemy.x == 1 && enemy.y <= 59 ) _enemy = enemy.move(0, 1)
      if(enemy.y == 59 && enemy.x <= 59) _enemy = enemy.move(1, 0)
      moveDelay = 0.0
    }
  }
}
