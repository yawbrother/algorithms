package com.gyesiama.algorithm

import scala.collection.mutable

object BreadthFirstSearch  extends App{
  type Graph[T] = Map[T, List[T]]

  def breadthFirstSearch[T](startFrom: T, graph: Graph[T], cond: (T) => Boolean): (Boolean, Option[T]) = {
    val searchQueue = mutable.Queue[T]()
    val searched = mutable.ArrayBuffer[T]()

    searchQueue ++= graph(startFrom)
    while(!searchQueue.isEmpty) {
      val person = searchQueue.dequeue()
      if(!searched.contains(person)) {
        if(cond(person)) {
          return (true, Some(person))
        } else {
          searchQueue ++= graph(person)
          searched :+ person
        }
      }
    }
    (false, None)
  }

  val graph = Map[String, List[String]](
    "you" -> List("alice", "bob", "claire"),
    "bob" -> List("anuj", "peggy"),
    "alice" -> List("peggy"),
    "claire" -> List("thom", "jonny"),
    "anuj" -> List(),
    "peggy" -> List(),
    "thom" -> List(),
    "jonny" -> List()
  )


  val found = breadthFirstSearch("you", graph, (name: String) => name.endsWith("m"))
  println(found)

}
