package fpinscala.chapter7

//import java.util.concurrent.ExecutorService
//
//import scala.concurrent.Future
import scala.concurrent.duration.TimeUnit

//trait Par[+A] {
//
//}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val (af, bf) = (pa(es), pb(es))
    UnitFuture(f(af.get, bf.get))
  }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = ???
    //map2(pa, pb)((a,b) => map2(pc, unit(()))((c,_) => f(a,b,c)))

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A] {
      def call: A = a(es).get
    })
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def run[A](e: ExecutorService)(a: Par[A]): Future[A] = a(e)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }



    def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }



  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if(ints.length <= 1)
      unit(ints.headOption getOrElse 0)
    else{
      val(i1, i2) = ints.splitAt(ints.length / 2)
      map2(fork(sum(i1)), fork(sum(i2)))(_ + _)
    }
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = sequence(ps.map(asyncF(f)))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit[List[A]](List()))((a,b) => map2(a,b)(_::_))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pa: List[Par[List[A]]] = as map asyncF((a) => if (f(a)) List(a) else Nil)
    map(sequence(pa))(_.flatten)
  }

  def equals[A,B](es: ExecutorService)(pa: Par[A], pb: Par[B]): Boolean = pa(es).get == pb(es).get

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => if(run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val i = run(es)(n).get
    run(es)(choices(i))
  }

  def choiceViachoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(a => if(a) 0 else 1))(List(t,f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es => {
    val k = run(es)(key).get
    run(es)(choices(k))
  }

  // was chooser
  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get
    run(es)(choices(a))
  }

  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get)

  def joinViaFlatMAp[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)

  def map2ViaFlatMap[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = flatMap(pa)(a => flatMap(pb)(b => unit(f(a,b))))


}

  abstract class ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
  }
  trait Callable[A] { def call: A }
  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }

