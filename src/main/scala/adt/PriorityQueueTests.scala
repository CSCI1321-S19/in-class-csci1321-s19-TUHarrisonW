package adt

object PriorityQueueTests extends App {
  //var pq: PriorityQueue[(Int,Int)] = null
  
  val pq = new SortedLLPQ[(Int,Int)](_._1 < _._1)
  
  def test2():Unit = {
    
    println("yes")
  }
  
  def test1():Unit = {
    pq.enqueue((1,2))
    pq.enqueue((1,5))
    pq.enqueue((1,3))
    println(pq.peek)
    pq.dequeue()
    println(pq.peek)
    pq.dequeue()
    println(pq.peek)
    pq.dequeue()
    println(pq.isEmpty)
  }
  test1
}