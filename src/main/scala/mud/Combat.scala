package MUD

import io.StdIn._

class Combat {
  
  def damage(): Int = {
    util.Random.nextInt(3)
  }
  def lootDrop(items: Array[Item]): Array[Item] = {
    Array.fill(util.Random.nextInt(2))(items(util.Random.nextInt(items.length)))
  }
  
}