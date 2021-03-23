import scala.annotation.tailrec

object MergeSort extends App {

  /**
   *
   * @param zs list to be mergeSorted
   * @tparam T type of array
   * @return mergeSorted array
   */
  def mergeSort[T <% Ordered[T]](zs: List[T]) : List[T] = {
    val n = zs.length / 2

    n match {
      case 0 => zs
      case _ =>  zs match {
        case Nil => zs
        case x :: xs => {
          def merge[T <% Ordered[T]] (xs: List[T], ys: List[T] ): List[T] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) => if (x < y) x :: merge (xs1, ys) else y :: merge (xs, ys1)
          }
          val (left, right) = zs.splitAt (n)
          merge (mergeSort (left), mergeSort (right) )
        }
      }
    }
  }



  def mergeSortTailRec[T <% Ordered[T]](zs: List[T]) : List[T] = {
    val n = zs.length / 2

    n match {
      case 0  => zs
      case _ =>  zs match {
        case Nil => zs
        case z :: zs1 => {
          @tailrec
          def merge(xs: List[T], ys: List[T], acc: List[T] ): List[T] = (xs, ys) match {
            case (Nil, ys) =>  acc ++ ys
            case (xs, Nil) => acc ++ xs
            case (x :: xs1, y :: ys1) => if (x < y) merge (xs1, ys, acc :+ x) else merge (xs, ys1, acc :+ y)
          }
          val (left, right) = zs.splitAt (n)
          merge (mergeSortTailRec (left), mergeSortTailRec (right), List())
        }
      }
    }
  }

  def mergeSortTailRec2[T](comp: (T, T) => Boolean)(xs: List[T]) : List[T] = {
    val n = xs.length / 2

    n match {
      case 0 => xs
      case _ =>  xs match {
        case Nil => xs
        case x :: xs1 => {
          @tailrec
          def merge(ys: List[T], zs: List[T], acc: List[T] ): List[T] = (ys, zs) match {
            case (Nil, ys) =>  acc ++ ys
            case (zs, Nil) => acc ++ zs
            case (y :: ys1, z :: zs1) => if (comp(y,z)) merge (ys1, zs, acc :+ y) else merge (ys, zs1, acc :+ z)
          }

          val (left, right) = xs splitAt n
          merge (mergeSortTailRec2 (comp)(left), mergeSortTailRec2 (comp)(right), List())
        }
      }
    }
  }

  val myList = List(5,1,4,6,9,10,100,30)
  println("raw list: " + myList)
  println("sorted - mergeSort: " + mergeSort(myList))
  println("sorted - mergeSortTailRec: " + mergeSortTailRec(myList))
  println("sorted - mergeSortTailRec2: " + mergeSortTailRec2((x: Int,y: Int) => x < y)(myList))


}
