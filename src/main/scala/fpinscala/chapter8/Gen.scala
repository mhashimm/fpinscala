package fpinscala.chapter8

import fpinscala.chapter5.Stream
import fpinscala.chapter6.{RNG, State}
import fpinscala.chapter8.Prop._


sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case object Proved extends Result {
  def isFalsified = false
}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def &&(p: Prop): Prop = Prop{(m, i, r) =>
     run(m, i, r) match {
       case Passed | Proved => p.run(m, i, r)
       case x => x
    }
  }

  def ||(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int
}


case class Gen[+A](sample: State[A, RNG]) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =  Gen(sample.flatMap(a => Gen.unit(f(a)).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap(n => listOfN(n))

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {
  def unit[A](a: A): Gen[A] = Gen(State(s => (a,s)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOf[A](g: Gen[A]): List[Gen[A]] = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n,rng) => randomStream(as)(rng).zipWithViaUnFold(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)}

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String = s"test case: $s\n" + s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(a => if(a) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }
}

case class SGen[+A](g: Int => Gen[A]){
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(g andThen ( _ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(g andThen (_ flatMap f))



}

object SGen {
  import Gen._
  //def **[A,B](s2: SGen[B]): SGen[(A,B)] = SGen(n => apply(n) ** s2(n))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  /*val sortedProp = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted
    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    (ns.isEmpty || nss.tail.isEmpty || !ns.zip(ns.tail).exists {
      case (a,b) => a > b
    })
    // Also, the sorted list should have all the elements of the input list,
    && !ns.exists(!nss.contains(_))
    // and it should have no elements not in the input list.
    && !nss.exists(!ns.contains(_))
  }*/


}


