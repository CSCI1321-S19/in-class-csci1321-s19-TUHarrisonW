package adt

import org.junit._
import org.junit.Assert._

class TestPriorityQueue {
  var pq: PriorityQueue[(Int,Int)] = null

  @Before
  def init() = {
    pq = new BinaryHeapPQ[(Int,Int)](_._1 < _._1)
  }

  @Test
  def emptyOnCreate = {
    assertTrue(pq.isEmpty)
  }

  @Test
  def addRemoveOne = {
    pq.enqueue((5,1))
    pq.enqueue((5,2))
    assertFalse(pq.isEmpty)
    assertEquals((5,1), pq.peek)
    assertEquals((5,1), pq.dequeue)
    assertEquals((5,2), pq.dequeue)
    assertTrue(pq.isEmpty)
  }

  @Test
  def addRemoveMany = {
    val nums = Array.fill(100)(util.Random.nextInt)
    for (n <- nums) pq.enqueue((n, n))
    for (n <- nums.sorted) {
      assertEquals((n,n), pq.peek)
      assertEquals((n,n), pq.dequeue)
    }
  }
  
  @Test
  def fillEmptyRefillReempty = {
    val nums = Array.fill(100)(util.Random.nextInt)
    for (n <- nums) pq.enqueue((n,n))
    for (n <- nums.sorted) {
      assertEquals((n,n), pq.peek)
      assertEquals((n,n), pq.dequeue)
    }
    val nums2 = Array.fill(100)(util.Random.nextInt)
    for (n <- nums2) pq.enqueue(n,n)
    for (n <- nums2.sorted) {
      assertEquals((n,n), pq.peek)
      assertEquals((n,n), pq.dequeue)
    }
  }
}