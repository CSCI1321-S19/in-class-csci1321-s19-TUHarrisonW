package graphicgame

import scala.collection.mutable.ListBuffer

class Level {
  private var moveDelay = 0.0
  private val moveInterval = 0.05
  private var _lstplayer = ListBuffer(new Player(1.0, 1.0, 1.0, 1.0))
  private var _lstenemy = ListBuffer(new Enemy(56,56,1.0,1.0), new Enemy(1, 56, 1.0, 1.0), new Enemy(50, 50, 1.0, 1.0))
  private var _lstbomb = ListBuffer[Bomb]()
  val maze = RandomMaze(3, false, 20, 20, 0.6)
  private var ents = ListBuffer(_lstplayer, _lstenemy, _lstbomb)

  
  def lstplayer = _lstplayer
  def lstenemy = _lstenemy
  def lstbomb = _lstbomb
  
  def entities: ListBuffer[Entity] = ents.flatten
  
  def intersectsp(enemy: Enemy, plr: Player): Unit = {
      if(Entity.intersect(enemy, plr)) {
        plr.kill()
        enemy.kill()
      }
  }
  def intersectsb(enemy: Enemy, bomb: Bomb): Unit = {
      if(Entity.intersect(enemy, bomb)) {
        bomb.kill()
        lstplayer(0).bombkill()
        enemy.kill()
        lstbomb -= _lstbomb(0)
        println(lstbomb.length)
        println(_lstbomb)
      }
  }
  def update(delay: Double): Unit = {
    if(lstplayer.length != 0) {
    moveDelay += delay
    if (moveDelay >= moveInterval ) { 
      if (lstplayer(0).lHeld)
        if(Entity.isClear(-1,0,lstplayer(0), maze)) _lstplayer(0) = lstplayer(0).move(-1, 0)
      if (lstplayer(0).rHeld)
        if(Entity.isClear(1, 0, lstplayer(0), maze)) _lstplayer(0) = lstplayer(0).move(1, 0) 
      if (lstplayer(0).uHeld)
        if(Entity.isClear(0, -1, lstplayer(0), maze)) _lstplayer(0) = lstplayer(0).move(0, -1)
      if (lstplayer(0).dHeld)
        if(Entity.isClear(0, 1, lstplayer(0), maze)) _lstplayer(0) = lstplayer(0).move(0, 1)
      if (lstplayer(0).sHeld) {
        if (lstplayer(0).bnumb == 0){
          if(_lstbomb.length != 0) {
            _lstbomb(0) = _lstbomb(0).spawn(lstplayer(0).x, lstplayer(0).y)
            lstbomb += _lstbomb(0)
            lstplayer(0).bombadd()
          } else {
            lstbomb += new Bomb(lstplayer(0).x, lstplayer(0).y, 1.0, 1.0)
            lstplayer(0).bombadd()
          }
        }
      }
      lstenemy(0) = lstenemy(0).shortMove(lstenemy(0), 0, maze, lstplayer(0))
      lstenemy(1) = lstenemy(1).shortMove(lstenemy(1), 1, maze, lstplayer(0))
      lstenemy(2) = lstenemy(2).shortMove(lstenemy(2), 2, maze, lstplayer(0))
      intersectsp(lstenemy(0), lstplayer(0))
      intersectsp(lstenemy(1), lstplayer(0))
      intersectsp(lstenemy(2), lstplayer(0))
      if(_lstbomb.length != 0) 
        intersectsb(lstenemy(0), lstbomb(0))
      if(_lstbomb.length != 0) 
        intersectsb(lstenemy(1), lstbomb(0))
      if(_lstbomb.length != 0) 
        intersectsb(lstenemy(2), lstbomb(0))
      if(!lstenemy(0).stillHere()) lstenemy(0) = new Enemy(59,59,1.0,1.0)
      if(!lstenemy(1).stillHere()) lstenemy(1) = new Enemy(59,59,1.0,1.0)
      if(!lstenemy(2).stillHere()) lstenemy(2) = new Enemy(1,59,1.0,1.0)
      if(!lstplayer(0).stillHere()) { 
        lstplayer -=_lstplayer(0)
      }
      moveDelay = 0.0
    }
    }
  }
}
