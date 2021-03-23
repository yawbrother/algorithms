package com.gyesiama.algorithm

import scala.annotation.tailrec

object BinarySearch  extends App {

  def binarySearch[T](cmp: (T, T) => Boolean)(arr: Array[T], item: T): Boolean = {
    @tailrec
    def binarySearch(ar: Array[T], elem: T, low: Int, high: Int): Option[Int] = {
      if (low > high) return None

      val mid = low + (high - low)/ 2
      if (ar(mid) == elem) Some(mid)
      else if(cmp(ar(mid), elem)) binarySearch(ar, elem, low, high - 1)
      else binarySearch(ar, elem, mid + 1, high)
    }

    binarySearch(arr, item, 0, arr.length) match {
      case Some(_) => true
      case _ => false
    }
  }


  val compare = (a: Int, b: Int) => a > b
  val binIntSearch = binarySearch(compare) _

  val testArray = Array(1,2,3,4,5,10,15)
  println(binIntSearch(testArray, 10))
  println(binIntSearch(testArray, -1))

}
