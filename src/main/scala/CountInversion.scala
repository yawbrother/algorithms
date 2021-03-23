import scala.annotation.tailrec

object CountInversion  extends App {

  def sortAndCountInv[T](comp: (T, T) => Boolean)(xs: List[T]): (List[T], Int) = {
    xs match {
      case Nil  => (xs, 0)
      case (x :: Nil)  => (xs, 0)
      case _ =>  {
        @tailrec
        def mergeAndCountSplitInv(bs: List[T], cs: List[T], acc: List[T], acc2: Int): (Int, List[T]) =
          (bs, cs) match {
            case (Nil, cs) =>  (acc2, acc ++ cs)
            case (bs, Nil) => (acc2 , acc ++ bs)
            case (b :: bs1, c :: cs1) => if (comp(b, c)) mergeAndCountSplitInv(bs1, cs, acc :+ b, acc2) else
              mergeAndCountSplitInv(bs, cs1, acc :+ c, acc2 + 1)
        }

        val (left, right) = xs.splitAt(xs.length / 2)
        val (ys, leftInv)  = sortAndCountInv(comp)( left)
        val (zs, rightInv) = sortAndCountInv(comp)( right)
        val (splitInv, us) = mergeAndCountSplitInv(ys, zs, List[T](), 0)
        (us, leftInv + rightInv + splitInv)
      }
    }
  }

  val result = sortAndCountInv((x: Int,y: Int) => x < y)(List(1,2,3))
  println(result)
  assert(0 == (result._2))

  val result2 = sortAndCountInv((x: Int,y: Int) => x < y)(List(1,20,6,4,5))
  println(result2)
  assert(5 == (result2._2))


}
