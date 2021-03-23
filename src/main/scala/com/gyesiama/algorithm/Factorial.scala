package com.gyesiama.algorithm

import scala.annotation.tailrec

object Factorial extends App {

  def factorial(n: Long): BigInt = {
    @tailrec
    def factorial(n: Long, acc: BigInt): BigInt = n match {
      case n if n <= 0 => acc
      case _ => factorial(n-1, n * acc)
    }

    factorial(n, 1)
  }

  println(for {
    n  <- 1 to 100
  } yield (n, factorial(n)))

}
