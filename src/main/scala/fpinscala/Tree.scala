package fpinscala


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(r) max depth(l))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v) => f(v)
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((a,b) => 1 + a max b)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((x,y) => Branch(x,y))
}
