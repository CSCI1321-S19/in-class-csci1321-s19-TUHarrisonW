package MUD

import io.StdIn._

class Combat {
  
  def hitmiss(): Int = {
    util.Random.nextInt(3)
  }
  def damage(a: Int, b: Int): Int = {
    //a = equip stat
    //b = level
    val hit = 1+a*(b+64)/640.0
    hit.toInt 
  }
  def lootDrop(items: Array[Item]): Array[Item] = {
    Array.fill(util.Random.nextInt(2))(items(util.Random.nextInt(items.length)))
  }
  //Max hit = 0.5 + A * (B+64) /640
  
}