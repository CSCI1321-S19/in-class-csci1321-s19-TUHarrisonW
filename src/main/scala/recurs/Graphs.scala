package recurs

import collection.mutable.Buffer

object Graphs extends App {
  val graph = Array(
      Array(0,1,0,1,1,0),
      Array(0,0,1,0,0,1),
      Array(1,0,0,0,1,0),
      Array(0,0,1,0,0,0),
      Array(0,0,0,0,0,1),
      Array(0,0,0,0,0,0))
      
   val mapgraph = Array(
       Array("-1","-1","Gob_House","-1","-1","-1"),                   //L_Castle
       Array("Chick_Coop","-1","-1","L_Castle","-1","-1"),         //Gob_House
       Array("Varrock_Mines","Gob_House","-1","-1","-1","-1"),        //Chick_Coop
       Array("Varrock_Square","Chick_Coop","-1","-1","-1","-1"),      //Varrock_Mines
       Array("Varrock_Castle","Varrock_Mines","-1","-1","-1","-1"),   //Varrock_Square
       Array("-1","Varrock_Square","-1","Grand_Exchange","-1","-1"),  //Varrock_Castle
       Array("-1","-1","Varrock_Castle","-1","-1","-1"))              //Grand_Exchange
      
   val directions = Map[Int, String](0 -> "north",1 -> "south", 2 -> "east", 3 -> "west", 4 -> "up", 5 -> "down")
   val roomnames = Map[String, Int]("L_Castle" -> 0, "Gob_House" -> 1, "Chick_Coop" -> 2, "Varrock_Mines" -> 3, "Varrock_Square" -> 4,"Varrock_Castle" -> 5, "Grand_Exchange" -> 6)
  
  def reachable(node1:String, node2:String,connect:Array[Array[String]], visited: Set[String] = Set.empty):Boolean = {
    if(node1 == node2) true else {
      val newVisited = visited + node1
      var i = 0
      var ret = false
      while(!ret && i < connect.length) {
        val nroom = connect(roomnames(node1))(i)
        if( connect(roomnames(node1))(i) != "-1" && !visited(nroom)){
          ret ||= reachable(nroom, node2, connect, newVisited)
        }
        i += 1
      }
      ret
    }
  }
  
  //println(reachable("L_Castle","Grand_Exchange",mapgraph))
  //println(reachable(4,3,graph))
  
  def shortestPath2(node1:Int,node2:Int,connect:Array[Array[Int]], visited: Set[Int] = Set.empty, theWay: List[Int] = List.empty): (Int,List[Int]) = {
    if(node1 == node2){
      //println(theWay.mkString(","))
      (0, theWay)
    } else {
      var ret = (100000000, List[Int]())
      val newVisited = visited + node1
      var i = 0
      while(i < connect.length) {
        if( connect(node1)(i) != 0 && !visited(i)) {
          val _theWay = theWay ::: List(i)
          //println(i)
          val go = shortestPath2(i, node2, connect, newVisited, _theWay)
          ret = (ret._1 min go._1, go._2)
        }
        i += 1
      }
      (ret._1 + 1, ret._2)
    }
  }
  
  //println(shortestPath2(0,5,graph)._2)
  //println(shortestPath2(4,3,graph))
  //println(shortestPath2(3,5,graph)._2.mkString(", "))
  
  private var path = Buffer[String]()
  
  def shortestPathMap(node1:String, node2:String, connect:Array[Array[String]], visited: Set[String] = Set.empty, theWay: List[String] = List.empty): (Int,List[String]) = {
    if(node1 == node2){
      (0, theWay)
    } else {
      var ret = (100000000, List[String]())
      val newVisited = visited + node1
      var i = 0
      while(i < 6) {
        val nroom = connect(roomnames(node1))(i)
        if( connect(roomnames(node1))(i) != "-1" && !visited(nroom)) {
          val _theWay = theWay ::: List(directions(i))
          val go = shortestPathMap(nroom, node2, connect, newVisited, _theWay)
          ret = (ret._1 min go._1, go._2)
        }
        i += 1
      }
      (ret._1 + 1, ret._2)
    }
  }
  
  println(shortestPathMap("L_Castle","Grand_Exchange",mapgraph)._2.mkString(", "))
}