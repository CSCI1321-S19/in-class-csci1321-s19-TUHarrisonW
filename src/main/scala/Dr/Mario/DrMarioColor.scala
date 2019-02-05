package Dr.Mario

object DrMarioColor extends Enumeration {
  val Red, Yellow, Blue = Value
  
  def random(): DrMarioColor.Value = {
    values.toSeq(util.Random.nextInt(values.size))
  }
}