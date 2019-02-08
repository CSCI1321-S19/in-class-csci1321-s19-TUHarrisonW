package graphicgame

class Level {
  private val fallInterval = 1.0
  private var fallDelay = 0.0
  private val moveInterval = 0.01
  private var moveDelay = 0.0
  private var _player = new Player(1.0, 1.0, 1.0, 1.0)
  private var _enemy = new Enemy(59,59,1.0,1.0)
  val maze = RandomMaze(3, false, 20, 20, 0.6)
  private var ents = List(_player, _enemy)

  def player = _player
  def enemy = _enemy
  
  //def maze: Maze = map //RandomMaze(3, false, 20, 20, 0.6)
  def entities: List[Entity] = ents
  def update(delay: Double): Unit = {
    fallDelay += delay
    moveDelay += delay
    if (moveDelay >= moveInterval) {
      if (player.lHeld)
        if(maze(player.x.toInt -1, player.y.toInt) != Wall) _player = player.move(-1, 0)
      if (player.rHeld)
        if(maze(player.x.toInt +1, player.y.toInt) != Wall) _player = player.move(1, 0) 
      if (player.uHeld)
        if(maze(player.x.toInt, player.y.toInt - 1) != Wall) _player = player.move(0, -1)
      if (player.dHeld)
        if(maze(player.x.toInt, player.y.toInt + 1) != Wall) _player = player.move(0, 1) 
      moveDelay = 0.0
    }
    
//    if (fallDelay >= fallInterval) {
//      if (currentPill.canMove(0, 1)) {
//        _currentPill = currentPill.move(0, 1)
//      } else {
//        _elements ::= _currentPill
//        _currentPill = nextPill
//        _nextPill = new Pill(List(
//          new PillPiece(3, 0, DrMarioColor.random()),
//          new PillPiece(4, 0, DrMarioColor.random())))
//      }
      fallDelay = 0.0
//    }
  }
}
