package fpinscala.chapter6


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if(n < 0) -(n + 1) else n, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = nonNegativeInt(r1)
    ((d,i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int)(rng: RNG)(list: List[Int]): (List[Int], RNG) = {
      if(count == 0) (list, rng)
      else {
        val (i, r) = nonNegativeInt(rng)
        loop(count - 1)(r)(i :: list)
      }
    }
    loop(count)(rng)(Nil)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (List(), rng)
    else {
      val (x,r) = rng.nextInt
      val (xs, r2) = ints2(count - 1)(rng)
      (x::xs, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(a => a - a % 2)

  def double1(rng: RNG): Rand[Double] = map(nonNegativeInt)(a => a.toDouble / (Int.MaxValue + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_,_))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case x::xs => map2(x, sequence(xs))(_::_)
    case _ => unit(Nil)
  }

  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((a,b) => map2(a,b)(_::_))

  def ints3(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =  rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

}

case class State[+A, S](run : S => (A,S)) {
  import State._
  def map[B](f: A => B): State[B,S] = flatMap(a => unit(f(a)))

  def map2[B](f: A => B): State[B,S] = State(s => {
    val (a, ss) = run(s)
    (f(a),ss)
  })

  def map2[B,C](sb: State[B,S])(f: (A, B) => C): State[C, S] = flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[B,S]): State[B,S] = State(s => {
    val (a,ss) = run(s)
    f(a).run(ss)
  })
}

object State {
  def unit[A,S](a: A): State[A,S] = State(s => (a,s))

  def sequence[A,S](ls: List[State[A,S]]): State[List[A], S] =
    ls.foldRight(unit[List[A],S](List()))((s,b) => s.map2(b)(_::_))

  def get[S]: State[S,S] = State(s => (s,s))

  def set[S](s: S): State[Unit, S] = State(_ => ((), s))

  def modify[S](f: S => S): State[Unit, S] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}