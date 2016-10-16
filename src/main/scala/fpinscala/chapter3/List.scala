package fpinscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail:List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def tailViaDrop[A](as: List[A]): List[A] = drop(1, as)

  def setHead[A](a: A, as: List[A]): List[A] = Cons(a, tail(as))

  def drop[A](n: Int, as: List[A]): List[A] = as match {
    case Cons(x, xs) if n != 0 => drop(n - 1, xs)
    case other => other
  }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case other => other
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => append(xs, Cons(x, a2))
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(x,xs) => f(x, foldRight(xs, z)(f))//foldRight(xs, f(x,z))(f)
    case _ => z
  }

  def sum2(as: List[Int]): Int = foldRight(as, 0)(_ + _)

  def product2(as: List[Double]): Double = foldRight(as, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_,b) => b + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
    case Nil => z
  }

  def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product3(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((_,b) => b + 1)

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def flatten[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(append)

  def add1(as: List[Int]): List[Int] = foldRight(as, Nil:List[Int])((a,b) => Cons(a + 1 ,b))

  def doubleToString(sa: List[Double]): List[String] = foldRight(sa, Nil:List[String])((a,b) => Cons(a.toString, b))

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))

  def map2[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x,xs) => Cons(f(x), map2(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x,xs) => filter(xs)(f)
  }

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((a,b) => if(f(a)) Cons(a, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))//foldRight(as, Nil:List[B])((a,b) => append(f(a), b))

  def filter3[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if(f(a)) List(a) else Nil)

  def addPair(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(x,xs), Cons(y, ys)) => Cons(x + y, addPair(xs, ys))
    case _ => Nil
  }

  def zipWith[A,B](as: List[A], bs: List[A])(f: (A,A) => B): List[B] = (as, bs) match {
    case (Cons(x,xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
    case _ => Nil
  }
}
