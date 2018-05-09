import scala.annotation.tailrec

// P01 (*) Find the last element of a list.

@tailrec
def lastL[T](list: List[T]) : Option[T] = list match {
  case Nil => None
  case h::Nil => Some(h)
  case h::t => lastL(t)
}
lastL(List(1,2,3))

@tailrec
def lastA[T](array: Array[T]) : Option[T] = array match {
  case Array() => None
  case Array(h) => Some(h)
  case Array(h,_*) => lastA(array.tail)
}
lastA(Array(1,2,3))

@tailrec
def lastS1[T](seq: Seq[T]) : Option[T] = seq match {
  case Seq() => None
  case h +: Seq() => Some(h)
  case h +: t => lastS1(t)
}
lastS1(Seq(1,2,3))

@tailrec
def lastS2[T](seq: Seq[T]) : Option[T] = seq match {
  case Nil => None
  case h :: Nil => Some(h)
  case h :: t => lastS2(t)
}
lastS2(Seq(1,2,3))

@tailrec
def lastV[T](vec: Vector[T]) : Option[T] = vec match {
  case IndexedSeq() => None
  case h +: IndexedSeq() => Some(h)
  case h +: t => lastV(t)
}
lastV(Vector(1,2,3))


// P05 (*) Reverse a list.

def reverseL[T](list: List[T]) : List[T] = list match {
  case Nil => Nil
  case h::t => reverseL(t) :+ h
}
reverseL(List(1,2,3))

def reverseA(array: Array[Int]) : Array[Int] = array match {
  case Array() => Array()
  case Array(h, _*) => reverseA(array.tail) :+ h// ++ Array(h)
}
reverseA(Array(1,2,3)).mkString(" ")


def reverseS1[T](seq: Seq[T]) : Seq[T] = seq match {
  case Seq() => Seq()
  case h +: t => reverseS1(t) :+ h
}
reverseS1(Seq(1,2,3))

def reverseV[T](vec: Vector[T]) : Vector[T] = vec match {
  case IndexedSeq() => Vector()
  case h +: t => reverseV(t) :+ h
}
reverseV(Vector(1,2,3))
