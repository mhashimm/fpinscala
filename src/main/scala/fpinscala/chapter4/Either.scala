package fpinscala.chapter4


sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case l @ Left(_) => l
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l @ Left(_) => l
    case Right(v) => f(v)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => b
    case r @ Right(_) => r
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(v), Right(z)) => Right(f(v,z))
    case (l @ Left(v), _)  => l
    case (_, l @ Left(v)) => l
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]

object Either {
  def Try[A](a: A): Either[Exception,A] =
    try Right(a)
    catch {case e: Exception => Left(e)}

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))((a,b) => a.map2(b)(_::_))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a,b) => f(a).map2(b)(_::_))

  def traverse1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x::xs => f(x).map2(traverse1(xs)(f))(_::_)
  }

  def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

}