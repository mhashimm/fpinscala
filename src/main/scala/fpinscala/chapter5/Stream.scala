package fpinscala.chapter5

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Cons(x, xs) => Some(x())
    case Empty => None
  }

  def toList: List[A] = this match {
    case Cons(x, xs) => x() :: xs().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(x, xs) if n != 0 => cons(x(), xs().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(x, xs) if n != 0 => xs().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(x, xs) if p(x()) => cons(x(), xs().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(x, xs) => p(x()) || xs().exists(p)
    case _ => false
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(x, xs) => f(x(), xs().foldRight(z)(f))
    case Empty => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if(p(a)) cons(a,b) else b)

  def headOptionViaFoldRight: Option[A] = foldRight[Option[A]](None)((a,b) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if(f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a,b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => f(a) append b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  //def mapViaUnFold[B](f: A => B): Stream[B] = unfold(empty[B]){case (a:A,s:Stream[B]) => Some((f(a), s))}

  def mapViaUnFold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(x,xs) => Some((f(x()), xs()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(x, xs), i) if i == 1 =>  Some((x(), (empty, i )))
    case (Cons(x, xs), i) =>  Some((x(), (xs(), i - 1)))
    case _ => None
  }

  def takeWhileViaUnFold(f: A => Boolean): Stream[A] = unfold(this){
    case Cons(x,xs) if f(x()) => Some((x(),xs()))
    case _ => None
  }

  def zipWithViaUnFold[B](that: Stream[B]): Stream[(A,B)] = unfold((this, that)){
    case (Cons(x,xs), Cons(y,ys)) => Some(((x(),y()), (xs(), ys())))
    case _ => None
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, that)){
    case (Cons(x,xs), Cons(y,ys)) => Some(((Some(x()),Some(y())), (xs(), ys())))
    case (Cons(x,xs), Empty) => Some(((Some(x()),None), (xs(), empty)))
    case (Empty, Cons(y,ys)) => Some(((None,Some(y())), (empty, ys())))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll{ case (x,y) => x == y }

  def tails: Stream[Stream[A]] = unfold((this)){
    case Empty => None
    case s => Some(s, s drop 1)
  } append(Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_.startsWith(s))

  //def scanRight(f: A: Stream[Stream[A]] = ???

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val hh = h
    lazy val tt = t
    Cons(() => hh, () => tt)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if(as.isEmpty) empty else cons( as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(curr: Int, prev: Int): Stream[Int] = cons(prev, loop(prev, prev + curr))
    loop(1, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case _ => empty
  }

  def constantViaUnFold[A](a: A): Stream[A] = unfold(empty[A])(s => Some((a,s)))

  def fromViaUnFold(n: Int): Stream[Int] = unfold(n){s => Some((s, s + 1))}

  def onesViaUnFold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  //def fibsViaUnfold: Stream[Int] = unfold((0,1))(s => Some((s._1 + s._2, (s._1 + s._2, s._1 ))))

  def fibsViaUnfold: Stream[Int] = unfold((0, 1)){case (s0, s1) => Some((s0, (s1, s0 + s1)))}

}