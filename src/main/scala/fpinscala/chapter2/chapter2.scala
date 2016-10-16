package fpinscala.chapter2

object chapter2 {

  def factorial(n: Int): Int = {
    def loop(i: Int, acc: Int): Int = {
      if(i <= 0) acc
      else loop(i - 1, i * acc)
    }
    loop(n, 1)
  }

  def fib(n: Int): Int = {
    def loop(n: Int, curr: Int, prev: Int): Int = {
      if(n == 0) curr
      else loop(n - 1, curr + prev, curr)
    }
    loop(n, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if(as.length <= 1) true
    else ordered(as.head, as.tail.head) && isSorted(as.tail, ordered)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a,b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
