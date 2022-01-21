package io.lambdaworks.workshop.recursion

import scala.annotation.tailrec

/**
  * Rewrite below non tail-recursive functions to tail-recursive one.
  * Add @tailrec annotation to prove it.
  */
object NonTail2TailRecursion {

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int =
      if (n <= 1) acc else loop(n - 1, n * acc)

    loop(n, 1)
  }

  def cubesOfEvens(numbers: List[Double]): List[Double] =
    numbers match {
      case x :: xs if x % 2 == 0 => Math.pow(x, 3) +: cubesOfEvens(xs)
      case _ :: xs => cubesOfEvens(xs)
      case Nil     => List.empty
    }

}
