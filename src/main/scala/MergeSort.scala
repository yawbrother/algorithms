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

  val myList = List(5,1,4,6,9,10,100,30)
  println("raw list: " + myList)
  println("sorted list: " + mergeSort(myList))

  def mergeSortTailRec[T <% Ordered[T]](zs: List[T]) : List[T] = {
    val n = zs.length / 2

    n match {
      case 0 => zs
      case _ =>  zs match {
        case Nil => zs
        case x :: xs => {
          @tailrec
          def merge[T <% Ordered[T]] (xs: List[T], ys: List[T], acc: List[T] ): List[T] = (xs, ys) match {
            case (Nil, ys) =>  acc ++ ys
            case (xs, Nil) => acc ++ xs
            case (x :: xs1, y :: ys1) => if (x < y) merge (xs1, ys, acc :+ x) else merge (xs, ys1, acc :+ y)
          }
          val (left, right) = zs.splitAt (n)
          merge (mergeSort (left), mergeSort (right), List())
        }
      }
    }
  }

  println("sorted: " + mergeSortTailRec(myList))







}
