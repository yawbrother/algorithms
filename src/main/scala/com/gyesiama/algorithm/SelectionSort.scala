package com.gyesiama.algorithm

import scala.annotation.tailrec

object SelectionSort  extends App {

  def sort[A](cmp: (A, A) => Boolean)(arr: Array[A]): Array[A] = {
    for(i <- 0 until arr.length) {
     for(j <- i+1 until arr.length) {
       if(!cmp(arr(i), arr(j))) {
         val tmp = arr(i)
         arr(i) = arr(j)
         arr(j) = tmp
       }
     }
    }
    arr
  }

  val sortInt = sort( (a: Int, b: Int) => a < b) _
  println(Array(1, 6, 4, -2, 5, 12).mkString("[", ",", "]"))
  println(sortInt(Array[Int](1, 6, 4, -2, 5, 12)).mkString("[", ",", "]"))

  def sortRec[T](cmp: (T,T) => Boolean)(xs: List[T]): List[T] = {
    def insert(y: T, ys: List[T]): List[T] = ys match {
      case Nil => y :: Nil
      case y1 :: xy1 if cmp(y, y1) => y :: ys
      case y2 :: ys2 => y2 :: insert(y, ys2)
    }

    xs match {
      case Nil => Nil
      case x :: xs1 => insert(x, sortRec(cmp)( xs1))
    }
  }

  val sortIntRec = sortRec( (a: Int, b: Int) => a < b) _
  println(sortIntRec(List[Int](1, 6, 4, -2, 5, 12)).mkString("[", ",", "]"))

  def sortTailRec[T](cmp: (T,T) => Boolean)(xs: List[T]): List[T] = {
    @tailrec
    def insert(y: T, ys: List[T], acc: List[T]): List[T] = ys match {
      case Nil =>  y :: acc
      case x :: xs1 if cmp(y, x) => acc ::: y :: x :: xs1
      case x :: xs1 => insert(y, xs1, acc :+ x)
    }

    xs match {
      case Nil => Nil
      case x :: xs1 => insert(x, sortTailRec(cmp)( xs1), List[T]())
    }
  }

  val sortIntTailRec = sortTailRec( (a: Int, b: Int) => a < b) _
  println(sortIntTailRec(List[Int](1, 6, 4, -2, 5, 12)).mkString("[", ",", "]"))

}
