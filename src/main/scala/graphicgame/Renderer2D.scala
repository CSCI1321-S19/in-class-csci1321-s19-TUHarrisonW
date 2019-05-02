package graphicgame

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.image.Image
import scalafx.scene.paint.Color

/**
 * This is a 2D renderer that with draw your game elements to a Canvas. You should change the
 * images to fit the style of your game. Also, alter the entities to match what you have in
 * your game.
 */
class Renderer2D(gc: GraphicsContext, blockSize: Double) {
  private var lastCenterX = 0.0
  private var lastCenterY = 0.0

  // Put variables for images here
  private val floorImage = Renderer2D.loadImage("/images/floor.png")
  private val wallImage = Renderer2D.loadImage("/images/wall.png")
  private val playerImage = Renderer2D.loadImage("/images/player.png")
  private val enemyImage = Renderer2D.loadImage("/images/enemy.png")
  private val generatorImage = Renderer2D.loadImage("/images/generator.png")
  private val bombImage = Renderer2D.loadImage("/images/bullet.png")
  

  /**
   * These two methods are used to figure out where to draw things. They are used by the render.
   */
  def blocksToPixelsX(bx: Double): Double = gc.canvas.getWidth / 2 + (bx - lastCenterX) * blockSize
  def blocksToPixelsY(by: Double): Double = gc.canvas.getHeight / 2 + (by - lastCenterY) * blockSize

  /**
   * These two methods are used to go from coordinates to blocks. You need them if you have mouse interactions.
   */
  def pixelsToBlocksX(px: Double): Double = (px - gc.canvas.getWidth / 2) / blockSize + lastCenterX
  def pixelsToBlocksY(py: Double): Double = (py - gc.canvas.getHeight / 2) / blockSize + lastCenterY

  /**
   * This method is called to render things to the screen.
   */
  def render(level: PassableLevel): Unit = {
    lastCenterX =  50
    lastCenterY =  40

    val drawWidth = (gc.canvas.getWidth / blockSize).toInt + 1
    val drawHeight = (gc.canvas.getWidth / blockSize).toInt + 1

     //Draw walls and floors
    for {
      x <- lastCenterX.toInt - drawWidth / 2 - 1 to lastCenterX.toInt + drawWidth / 2 + 1
      y <- lastCenterY.toInt - drawHeight / 2 - 1 to lastCenterY.toInt + drawHeight / 2 + 1
    } {
      val img = level.maze(x, y) match {
        case Wall => wallImage
        case Floor => floorImage
      }
      //gc.drawImage(img, blocksToPixelsX(x), blocksToPixelsY(y), blockSize, blockSize)
      if(img == wallImage){
        gc.fill = Color.Black 
        gc.fillRect(blocksToPixelsX(x), blocksToPixelsY(y), blockSize, blockSize)
      } else         
        gc.fill = Color.White 
        gc.fillRect(blocksToPixelsX(x), blocksToPixelsY(y), blockSize, blockSize)
    }

     //Draw entities
    for (e <- level.entities) {
      val img = e.style match {
        case 1 => { 
          gc.fill = Color.Green 
          gc.fillRect(blocksToPixelsX(e.x), blocksToPixelsY(e.y), blockSize, blockSize) //playerImage
          gc.fill = Color.White
          gc.fillText("Score: "+e.score, blocksToPixelsX(54), blocksToPixelsX(62))
        }
        case 2 => { //enemyImage 
          gc.fill = Color.Red 
          gc.fillRect(blocksToPixelsX(e.x), blocksToPixelsY(e.y), blockSize, blockSize)
        }
        case 3 => {
          gc.fill = Color.Blue 
          gc.fillRect(blocksToPixelsX(e.x), blocksToPixelsY(e.y), blockSize, blockSize)
        }
        case 4 => {
          gc.fill = Color.Purple 
          gc.fillRect(blocksToPixelsX(e.x), blocksToPixelsY(e.y), blockSize, blockSize)
        }
        case m => {
          println("Render2D: Case not delt with: "+e)
        }
//        case g: Generator => generatorImage
      }
//      if(level.maze.wrap) {
//        for(rx <- -1 to 1; ry <- -1 to 1)
//    	    gc.drawImage(img, blocksToPixelsX(e.x-e.width/2+rx*level.maze.width), blocksToPixelsY(e.y-e.height/2+ry*level.maze.height), e.width*blockSize, e.height*blockSize)
//      } else {
//    	  gc.drawImage(img, blocksToPixelsX(e.x-e.width/2), blocksToPixelsY(e.y-e.height/2), e.width*blockSize, e.height*blockSize)
//      }
//    }
    }
  }
}

object Renderer2D {
  /**
   * This method assumes that you are putting your images in src/main/resources. This directory is
   * packaged into the JAR file. Eclipse doesn't use the JAR file, so this will go to the file in
   * the directory structure if it can't find the resource in the classpath. The argument should be the
   * path inside of the resources directory.
   */
  def loadImage(path: String): Image = {
    val res = getClass.getResource(path)
    if(res == null) {
      new Image("file:src/main/resources"+path)
    } else {
      new Image(res.toExternalForm())
    }
  }  
}