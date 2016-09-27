package lensprotocol

import monocle._

trait ConsProtocol {
  type Cons

  type Head
  type Tail
}



trait ConsOptics[CProtocol <: ConsProtocol] {
  val consProtocol: CProtocol

  import consProtocol._

  def consHead: Lens[Cons, Head]
  def consTail: Lens[Cons, Tail]
  def cons: Iso[Cons, (Head, Tail)]

  // laws:
  //   consHead ~~ consHeadTail._1
  //   consTail ~~ consHeadTail._2
  //   consHeadTail ~~ (consHead, consTail)

  implicit class Methods(c: Cons) {
    def head: Head = consHead.get(c)
    def tail: Tail = consTail.get(c)
  }

  object Cons {
    def apply(h: Head, t: Tail): Cons = cons.reverseGet(h, t)
    def unapply(c: Cons): Option[(Head, Tail)] = Some(cons.get(c))
  }
}

/**
  * The cons-list protocol.
  *
  * @author Matthew Pocock
  */
trait ConsListProtocol {

  type ConsList

}

trait ConsListConsProtocol extends ConsListProtocol {
  type Cons
}

trait ConsListConsOptics[CLCProtocol <: ConsListConsProtocol] {
  val clcProtocol: CLCProtocol

  import clcProtocol._

  def consListCons: Prism[ConsList, Cons]

  implicit class Upcaster(c: Cons) {
    implicit def upcast: ConsList = consListCons.reverseGet(c)
  }
}

trait ConsListNilProtocol extends ConsListProtocol {
  type Nil
}

trait ConsListNilOptics[CLNProtocol <: ConsListNilProtocol] {
  val clnProtocol: CLNProtocol

  import clnProtocol._

  def consListNil: Prism[ConsList, Nil]

  implicit class Upcaster(n: Nil) {
    implicit def upcast: ConsList = consListNil.reverseGet(n)
  }
}

trait consListConsOrNilOptics[CLCNProtocol <: ConsListConsProtocol with ConsListNilProtocol] {
  val clcProtocol: CLCNProtocol

  import clcProtocol._

  def fold[A](cl: ConsList)(atCons: Cons => A, atNil: Nil => A): A
}

object ConsListProtocol {

  import scala.language.higherKinds

  type listProtocol[L[_], LCons[_], LNil, H] = ConsListConsProtocol with ConsListNilProtocol with ConsProtocol {
    type ConsList = L[H]
    type Cons = LCons[H]
    type Nil = LNil
    type Head = H
    type Tail = L[H]
  }

  object hList {
    type consListProtocol[HList] = ConsListProtocol {
      type ConsList = HList
    }

    type consListConsProtocol[HList, HCons[_, _], H, T] = consListProtocol[HList] with ConsListConsProtocol {
      type Cons = HCons[H, T]
    }

    type consListNilProtocol[HList, HNil] = consListProtocol[HList] with ConsListNilProtocol {
      type Nil = HNil
    }

    type consProtocol[HCons[_, _], H, T] = ConsProtocol {
      type Cons = HCons[H, T]
      type Head = H
      type Tail = T
    }
  }
}