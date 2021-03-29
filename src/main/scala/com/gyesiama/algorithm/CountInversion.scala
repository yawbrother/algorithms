package com.gyesiama.algorithm

import scala.annotation.tailrec

object CountInversion extends App {

  def sortAndCountInv[T](comp: (T, T) => Boolean)(xs: List[T]): (List[T], Int) = {
    @tailrec
    def mergeAndCountSplitInv(bs: List[T], cs: List[T], acc: List[T], acc2: Int): (Int, List[T]) = (bs, cs) match {
      case (_, Nil) => (acc2, bs.reverse ::: acc)
      case (Nil, _) => (acc2, cs.reverse ::: acc)
      case (b :: bs1, c :: cs1) => {
        if (comp(b, c)) mergeAndCountSplitInv(bs1, cs,  b :: acc, acc2) else
          mergeAndCountSplitInv(bs, cs1, c :: acc, acc2 + bs.size)
      }
    }

    xs match {
      case Nil => (xs, 0)
      case (x :: Nil) => (xs, 0)
      case _ => {
        val (left, right) = xs.splitAt(xs.length / 2)
        val (ys, leftInv) = sortAndCountInv(comp)(left)
        val (zs, rightInv) = sortAndCountInv(comp)(right)
        val (splitInv, us) = mergeAndCountSplitInv(ys, zs, List[T](), 0)
        (us.reverse, leftInv + rightInv + splitInv)
      }
    }
  }

  val result = sortAndCountInv((x: Int, y: Int) => x < y)(List(1, 2, 3))
  println(result)
  assert(0 == (result._2))

  val result2 = sortAndCountInv((x: Int, y: Int) => x < y)(List(1, 20, 6, 4, 5))
  println(result2)
  assert(5 == (result2._2))


}
