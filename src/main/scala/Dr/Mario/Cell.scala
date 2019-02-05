package Dr.Mario

trait Cell {
  def x: Int
  def y: Int
  def color: DrMarioColor.Value
  
  def supported(): Boolean
}