package com.gyesiama.algorithm

import scala.util.Random

object QuickSort extends App {
  def quickSort[A](xs: List[A])(cmp: (A, A) => Boolean): List[A] = {
    val r = Random
    xs match {
      case Nil => xs
      case x :: Nil => xs
      case _ => {
        val pivotIndex: Int = r.nextInt(xs.length)
        val pivot = xs(pivotIndex)
        val (less, greater)  = xs.partition(cmp(_, pivot)) // Note partition includes the pivot
        quickSort(less)(cmp) :::  quickSort(greater)(cmp)
      }
    }
  }

  val myList = List(5, 1, 4, 6, 9, 10, 100, 30, -1)
  println("raw list: " + myList)
  println("sorted - quick sort: " + quickSort(myList)((a,b) => a < b))
}

