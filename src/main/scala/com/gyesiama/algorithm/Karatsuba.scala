package com.gyesiama.algorithm

object Karatsuba extends App {

  def recIntMult(x: String, y: String): String = {
    def maxLength(a: String, b: String) = if (x.length < y.length) y.length else x.length

    if (maxLength(x, y) == 1) (x.toLong * y.toLong).toString else {
      val (a, b) = x.splitAt(x.length / 2)
      val (c, d) = y.splitAt(y.length / 2)

      val ac = recIntMult(a, c)
      val ad = recIntMult(a, d)
      val bc = recIntMult(b, c)
      val bd = recIntMult(b, d)

      (BigInt(ac) * BigInt("10").pow(x.length) +
        (BigInt(ad) + BigInt(bc)) * BigInt("10").pow(x.length / 2) +
        BigInt(bd)).toString
    }
  }

  assert(recIntMult("50", "10") == "500")
  assert(recIntMult("5678", "1234") == "7006652")

  val result = recIntMult("3141592653589793238462643383279502884197169399375105820974944592",
    "2718281828459045235360287471352662497757247093699959574966967627")
  println(result)

  val fromBigInt = BigInt("3141592653589793238462643383279502884197169399375105820974944592") *
    BigInt("2718281828459045235360287471352662497757247093699959574966967627")
  assert(result == fromBigInt.toString)
}
