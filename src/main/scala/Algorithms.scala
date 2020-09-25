import scala.annotation.tailrec

object Algorithms  extends App {

  //binary search
  def binarySearch[T](array: Array[T], item: T, greaterThan: (T, T) => Boolean): Boolean = {
    @tailrec
    def binarySearchRecursive[T](arr: Array[T], elem: T, low: Int, high: Int, grtThan: (T, T) => Boolean): Option[Int] = {
      if(low > high) return None

      val mid = low + (high - low)/ 2
      if (arr(mid) == elem) Some(mid)
      else if(grtThan(arr(mid), elem)) binarySearchRecursive(arr, elem, low, high - 1, grtThan)
      else binarySearchRecursive(arr, elem, mid + 1, high, grtThan)
    }

    binarySearchRecursive(array, item, 0, array.length, greaterThan) match {
      case Some(value) => true
      case _ => false
    }
  }

  val compare = (a: Int, b: Int) => a > b
  val testArray = Array(1,2,3,4,5,10,15)
  println(binarySearch(testArray, 10,compare))
  println(binarySearch(testArray, -1,compare))

  //factorial
  def factorial(n: Int) = {
    @tailrec
    def factorialHelper(n: Int, accu: Int): Int = n match {
      case n if n <= 0 => accu
      case _ => factorialHelper(n-1, n * accu)
    }

    factorialHelper(n, 1)
  }

  println(factorial(4))
  println(factorial(10))

  //fibonacci
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => go(n-1, b, a + b)
    }
    go(n, 0, 1)
  }

  println(fib(4))

  val fibonacci = for {
    n <- List.range(1, 10)
  } yield fib(n)

  println(fibonacci)
}
