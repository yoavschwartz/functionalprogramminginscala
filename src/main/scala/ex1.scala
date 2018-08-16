package main.scala

object main {
    def fib(n: Int): Int = {
      def go(n: Int):Int = {
        if (n == 1) 1
        else if (n == 0) 0
        else go(n-1) + go(n-2)
      }
      go(n)
    }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n+1 >= as.length) true
      else ordered(as(n), as(n+1)) && loop(n+1)
    }
    return loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
//    (a: A) => (b: B) => f(a,b)
    (a: A) => partial1(a, f)
  }

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    def funct (a: Int): Int = { a + 1  }
      println(compose(funct, funct)(2))
    }
}
