package ch.ethz.inf.pm.semper.chalice2sil.translation.operators

import collection.immutable

sealed abstract class Lookup[+T] {
  def map[R](f : T => R) : Lookup[R]
  def filter(p : T => Boolean) : Lookup[T]
  def toList : immutable.List[T]
  def toOption : Option[T]
  def orElse[R >: T](e : => R) : R
}

/**
  * @author Christian Klauser
  */

object Lookup {

  def fromList[T](candidates : immutable.List[T]) : Lookup[T] = candidates match {
    case Nil => Failure()
    case List(op) => Success(op)
    case cs => Ambiguous(cs)
  }

  final case class Failure() extends Lookup[Nothing] {
    override def map[R](f : (Nothing) => R) = this
    override def filter(p : (Nothing) => Boolean) = this

    override def toList = Nil

    override def toOption = None

    override def orElse[R >: Nothing](e :  => R) = e
  }

  final case class Success[T](operator : T) extends Lookup[T] {
    override def map[R](f : T => R) = Success(f(operator))
    override def filter(p : T => Boolean) = if (p(operator)) this else Failure()
    override def toList = List(operator)
    override def toOption = Some(operator)
    override def orElse[R >: T](e : => R) = operator
  }
  final case class Ambiguous[T] private[Lookup] (candidates : immutable.List[T]) extends Lookup[T] {
    require(candidates.length > 1)
    override def map[R](f : T => R) = fromList[R](candidates map f)
    override def filter(p : T => Boolean) = candidates filter p match {
      case Nil => Failure()
      case List(op) => Success(op)
      case cs if cs == candidates => this
      case cs => Ambiguous(cs)
    }
    override def toList = candidates
    override def toOption = None
    override def orElse[R >: T](e : => R) = e
  }
}
