package mud

class SortedLLPQ[A](higherP: (A,A) => Boolean) extends PriorityQueue[A] {
  private var default: A = _
  private class Node(var data: A, var prev: Node, var next: Node)
  private val end: Node = new Node(default, null, null)
  end.prev = end
  end.next = end
  
  def enqueue(a: A): Unit = {
    var rover = end.prev
    while(rover != end && higherP(a, rover.data)) rover = rover.prev
    rover.next.prev = new Node(a, rover, rover.next)
    rover.next = rover.next.prev
  }
  
  def dequeue(): A = {
    val ret = end.next.data
    end.next.next.prev = end
    end.next = end.next.next
    ret
  }
  
  def peek: A = end.next.data
  
  def isEmpty: Boolean = end.next == end
  
}