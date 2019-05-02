package graphicgame

import scala.collection.mutable.ListBuffer
import scalafx.scene.input.KeyCode

class Level {
  private var moveDelay = 0.0
  private val moveInterval = 0.1
  private var bmoveDelay = 0.0
  private val bmoveInterval = 0.5
  private var _lstplayerOrder = ListBuffer[String]()
  private var _lstplayer = ListBuffer[Player]()
  private var _lstenemy = ListBuffer[Enemy]()
  private var _lstboss = ListBuffer[Boss]()
  private var lstbombOrder = ListBuffer[String]()
  private var _lstbomb = ListBuffer[Bomb]()
  private val maze = RandomMaze(3, false, 20, 20, 0.6)
  private var ents = ListBuffer(_lstplayer, _lstenemy, _lstbomb, _lstboss)
  private var checkSupport = false
  private var moves = false
  
  def drawCurrent = !checkSupport
  
  def lstplayer = _lstplayer
  def lstenemy = _lstenemy
  def lstbomb = _lstbomb
  def lstboss = _lstboss
  def lstplayerOrder = _lstplayerOrder
  
  def entities: ListBuffer[Entity] = ents.flatten
  
  def addPlr(name: String): Unit = {
    lstplayer += new Player(1.0, 1.0, 1.0, 1.0, 0)
    lstplayerOrder += name
    lstenemy += new Enemy(50,50,1.0,1.0, lstplayerOrder.indexOf(name))
    lstenemy += new Enemy(56, 50, 1.0, 1.0, lstplayerOrder.indexOf(name))
    lstboss += new Boss(56, 56, 1.0, 1.0, lstplayerOrder.indexOf(name))
  }
  
  def makePassable(): PassableLevel = {
    PassableLevel(ents.flatMap(_.map(_.makePassable())), maze,drawCurrent)
  }
  
  def intersectsp(enemy: Enemy, plr: Player): Unit = {
      if(Entity.intersect(enemy, plr)) {
        plr.kill()
        enemy.kill()
      }
  }
  def intersectsb(enemy:  Enemy, bomb: Bomb, name:String): Unit = {
      if(Entity.intersect(enemy, bomb)) {
        bomb.kill()
        lstplayer(lstplayerOrder.indexOf(name)).bombkill()
        enemy.kill()
        lstplayer(lstplayerOrder.indexOf(name)).addscore()
        lstbomb -= _lstbomb(lstbombOrder.indexOf(name))
        lstbombOrder -= name
      }
  }
  def handleKey(obj: AnyRef,name: String ): Unit = {
    obj match { 
      case UpPressed => lstplayer(lstplayerOrder.indexOf(name)).upPressed()
      case DownPressed => lstplayer(lstplayerOrder.indexOf(name)).downPressed()
      case LeftPressed => lstplayer(lstplayerOrder.indexOf(name)).leftPressed()
      case RightPressed => lstplayer(lstplayerOrder.indexOf(name)).rightPressed()
      case SpacePressed => lstplayer(lstplayerOrder.indexOf(name)).spacePressed()
      case UpReleased => lstplayer(lstplayerOrder.indexOf(name)).upReleased()
      case DownReleased => lstplayer(lstplayerOrder.indexOf(name)).downReleased()
      case LeftReleased => lstplayer(lstplayerOrder.indexOf(name)).leftReleased()
      case RightReleased => lstplayer(lstplayerOrder.indexOf(name)).rightReleased()
      case SpaceReleased => lstplayer(lstplayerOrder.indexOf(name)).spaceReleased()
    }
  }
  
  def update(delay: Double, name:String): Boolean = {
    if(!lstplayer.isEmpty && lstplayerOrder.indexOf(name) != -1) {
    moveDelay += delay
    bmoveDelay += delay
    if (moveDelay >= moveInterval) { 
      lstplayer(lstplayerOrder.indexOf(name)) = lstplayer(lstplayerOrder.indexOf(name)).playerMove(lstplayer(lstplayerOrder.indexOf(name)), maze)
      if(lstplayer(lstplayerOrder.indexOf(name)).sHeld){
        if (lstplayer(lstplayerOrder.indexOf(name)).bnumb == 0) {
          if(lstbombOrder.indexOf(name) == -1) {
            lstbombOrder += name
            lstbomb += lstplayer(lstplayerOrder.indexOf(name)).placeBomb()
          }
          else{ 
            lstbomb(lstbombOrder.indexOf(name)) = lstplayer(0).placeBomb()
          }
        }
      }
      
      for(i <- 0 until lstenemy.length) {
        lstenemy(i) = lstenemy(i).shortMove(lstenemy(i), 0, maze, lstplayer(lstenemy(i).who))
        intersectsp(lstenemy(i), lstplayer(lstenemy(i).who))
        if(_lstbomb.length != 0 && lstbombOrder.indexOf(name) != -1) 
          intersectsb(lstenemy(i), lstbomb(lstbombOrder.indexOf(name)), name)
        if(!lstenemy(i).stillHere()) { 
          lstenemy(i) = lstenemy(i).respawn()
        }
      }
      for(i <- 0 until lstplayer.length) {
        if(lstplayer.length > i) {
          //println(lstplayer.length-1)
          if(!lstplayer(i).stillHere()) { 
            lstplayer -= _lstplayer(i)
            lstplayerOrder -= _lstplayerOrder(i)
            //_lstplayer = ListBuffer[Player]()
            lstboss -= _lstboss(3*i)
            lstenemy -= _lstenemy(3*i+1)
            lstenemy -= _lstenemy(3*i)
            lstenemy.foreach(_.changewho(lstplayerOrder.indexOf(name)))
            for(j <- 0 until lstplayerOrder.length) {
              _lstboss(3*j).changewho(lstplayerOrder.indexOf(name))
              _lstenemy(3*j+1).changewho(lstplayerOrder.indexOf(name))
              _lstenemy(3*j).changewho(lstplayerOrder.indexOf(name))
              //println(lstenemy(j).who)
            }
          }
        }
      }
      if(bmoveDelay >= bmoveInterval) {
        for(i <- 0 until lstboss.length) {
          lstboss(i) = lstboss(i).shortMove(lstboss(i), 0, maze, lstplayer(lstenemy(i).who))
          if(Entity.intersect(lstboss(i), lstplayer(lstboss(i).who))) {
            lstplayer(lstenemy(i).who).kill
          }
        }
        bmoveDelay = 0.0
      }
      moveDelay = 0.0
      true
    } else false
    } else false
  }
}
