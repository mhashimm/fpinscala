import fpinscala.{Cons, List, Nil}

import List._

val list = List(1,2,3,4,5,6,7)

val ll = List(List(1), List(2,3), List(4))

zipWith(List(1,2,3), List(4,5,6))(_ * _)

