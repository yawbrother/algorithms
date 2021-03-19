import scala.annotation.tailrec

object Fibonacci {

  def fibonacci(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fibonacci(n -1) + fibonacci(n-2)
  }

  def fibonacciArr(n: Int): Int = {

    n match {
      case 0 | 1 => n
      case _ => {
        val arr = new Array[Int](n +1)
        arr(0) = 0
        arr(1) = 1
        for(i <- 2 to n) {
          arr(i) = arr(i-1) + arr(i-2)
        }
        arr(n)
      }
    }
  }

  def fibonacciSeq(n: Int): Int = {

    n match {
      case 0 => 0
      case 1 => 1
      case x if x < 0 => 0
      case _ => {
        var fn2 = 0
        var fn1 = 1
        var res = 0
        for(i <- 2 to n) {
          res = fn1 + fn2
          fn2 = fn1
          fn1 = res
        }
        res
      }
    }
  }

  def fibonacciElegant(n: Int): Int = {
    @tailrec
    def fibTail(n: Int, a: Int, b: Int) : Int = n match {
      case 0 => a
      case _ => fibTail(n-1, b, a+b)
    }
    fibTail(n, 0, 1)
  }

  def main(args: Array[String]) {
    println("fib(2): " + fibonacci(2))
    println("fib(2): " + fibonacciArr(2))
    println("fib(2): " + fibonacciSeq(2))
    println("fib(2): " + fibonacciElegant(2))

    println("fib(3): " + fibonacci(3))
    println("fib(3): " + fibonacciArr(3))
    println("fib(3): " + fibonacciSeq(3))
    println("fib(3): " + fibonacciElegant(3))

    println("fib(4): " + fibonacci(4))
    println("fib(4): " + fibonacciArr(4))
    println("fib(4): " + fibonacciSeq(4))
    println("fib(4): " + fibonacciElegant(4))

    println("fib(5): " + fibonacci(5))
    println("fib(5): " + fibonacciArr(5))
    println("fib(5): " + fibonacciSeq(5))
    println("fib(5): " + fibonacciElegant(5))

    println("fib(6): " + fibonacci(6))
    println("fib(6): " + fibonacciArr(6))
    println("fib(6): " + fibonacciSeq(6))
    println("fib(6): " + fibonacciElegant(6))



  }

}
