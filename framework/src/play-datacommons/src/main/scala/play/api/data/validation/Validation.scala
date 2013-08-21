package play.api.data.validation

/**
* Validation[E, A] is the result of a validation, where E is the type of each error, and A is the type of the result if the validation is successful
* The only two possible implementations are Success[E, A](value: A), or Failure[E, A](errors: Seq[E])
*/
sealed trait Validation[+E, +A] { self =>

  /**
  * [use case] Builds a new Validation by applying a function to the value of this validation if it's a Success
  * {{{
  *   val f: Int => Int = _ + 2
  *   Success(5).map(f) == Success(7)
  *   Failure(Seq("error")).map(f) == Failure(Seq("error"))
  * }}}
  * @param f the function to apply if this is a `Success`
  * @return the result of applying the function
  */
  def map[X](f: A => X): Validation[E, X] = this match {
    case Success(v) => Success(f(v))
    case Failure(e) => Failure(e)
  }

  /**
  * [use case] Builds a new Validation by applying a function to the value of this validation if it's a Success
  * and using the resulting Validation.
  * {{{
  *   val f: Int => Validation[String, Int] = x => Success(x + 2)
  *   Success(5).flatMap(f) == Success(7)
  *   Failure(Seq("error")).flatMap(f) == Failure(Seq("error"))
  *
  *   val fe: Int => Validation[String, Int] = _ => Failure(Seq("don't panic"))
  *   Success(5).flatMap(fe) == Failure(Seq("don't panic"))
  *   Failure(Seq("error")).flatMap(fe) == Failure(Seq("error"))
  * }}}
  * @param f the function to apply if this is a `Success`
  * @return the result of applying the function
  */
  def flatMap[EE >: E, AA](f: A => Validation[EE, AA]): Validation[EE, AA] = this match {
    case Success(v) => f(v)
    case Failure(e) => Failure(e)
  }

  /**
  * Applies `invalid` if this is a Failure or `valid` if this is a Success.
  * @param invalid the function to apply if this is a `Failure`
  * @param valid the function to apply if this is a `Success`
  * @return the results of applying the function
  */
  def fold[X](invalid: Seq[E] => X, valid: A => X): X = this match {
    case Success(v) => valid(v)
    case Failure(e) => invalid(e)
  }

  def filterNot[EE >: E](error: EE)(p: A => Boolean): Validation[EE, A] =
    this.flatMap { a => if (p(a)) Failure[EE, A](Seq(error)) else Success[EE, A](a) }

  def filterNot(p: A => Boolean): Validation[E, A] =
    this.flatMap { a => if (p(a)) Failure[E, A](Nil) else Success[E, A](a) }

  /**
  * filter Successful Validation if it does not match the predicate `p`
  * @param p the predicate to apply if this is a `Success`
  * @return a Success if this was a Success and the predicate matched, a Failure otherwise
  */
  def filter(p: A => Boolean): Validation[E, A] =
    this.flatMap { a => if (p(a)) Success[E, A](a) else Failure[E, A](Nil) }

  /**
  * filter Successful Validation if it does not match the predicate `p`
  * {{{
  *   val isFive: Int => Boolean = _ == 5
  *   Success(5).filter("Not five")(isFive) == Success(5)
  *   Success(7).filter("Not five")(isFive) == Failure(Seq("Not five"))
  *   Failure(Seq("error")).filter("Not five")(isFive) == Failure(Seq("error"))
  * }}}
  * @param otherwise the error to return if the predicate `p` is not verified
  * @param p the predicate to apply if this is a `Success`
  * @return a Success if this was a Success and the predicate matched, a Failure otherwise
  */
  def filter[EE >: E](otherwise: EE)(p: A => Boolean): Validation[EE, A] =
    this.flatMap { a => if (p(a)) Success[EE, A](a) else Failure[EE, A](Seq(otherwise)) }

  /**
  * Like `map`, but for partial function. If `p` us not defined for the value, it return a Failure
  * {{{
  *   val p: PartialFunction[Int, String] = { case 5 => "High five!" }
  *   Success(5).collect("OOoops")(p) == Success("High five!")
  *   Success(7).collect("OOoops")(p) == Failure(Seq("OOoops"))
  *   Failure(Seq("error")).collect("OOoops")(p) == Failure(Seq("error"))
  * }}}
  * @param otherwise the error to return if the `p` is not defined
  * @param p the partial function to apply if this is a `Success`
  * @return a Success if this was a Success and `p` was defined, a Failure otherwise
  */
  def collect[EE >: E, B](otherwise: EE)(p: PartialFunction[A, B]): Validation[EE, B] = flatMap {
    case t if p.isDefinedAt(t) => Success(p(t))
    case _ => Failure(Seq(otherwise))
  }

  /**
  * Applies the given function `f` if this is a Success, otherwise returns Unit if this is a Failure
  * @param f the function to apply if this is a `Success`
  * @return Unit
  */
  def foreach(f: A => Unit): Unit = this match {
    case Success(a) => f(a)
    case _ => ()
  }

  /** Creates a non-strict filter of this Validation.
  *
  *  Note: the difference between `c filter p` and `c withFilter p` is that
  *        the former creates a new vlaidation, whereas the latter only
  *        restricts the domain of subsequent `map`, `flatMap`, `foreach`,
  *        and `withFilter` operations.
  *
  *  @param p   the predicate used to test value.
  *  @return    an object of class `WithFilter`, which supports
  *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
  *             All these operations apply to the value of this Validation
  *             which satisfy the predicate `p`.
  */
  def withFilter(p: A => Boolean) = new WithFilter(p)

  final class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Validation[E, B] = self match {
      case Success(a) =>
        if (p(a)) Success(f(a))
        else Failure(Nil)
      case Failure(errs) => Failure(errs)
    }
    def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B] = self match {
      case Success(a) =>
        if (p(a)) f(a)
        else Failure(Nil)
      case Failure(errs) => Failure(errs)
    }
    def foreach(f: A => Unit): Unit = self match {
      case Success(a) if p(a) => f(a)
      case _ => ()
    }
    def withFilter(q: A => Boolean) = new WithFilter(a => p(a) && q(a))
  }

  /**
  * Returns the value from this Success or throws the exception if this is a Failure.
  */
  def get: A

  /**
  * Returns the value from this Success or returns `t` if this is a Failure.
  */
  def getOrElse[AA >: A](t: => AA): AA = this match {
    case Success(a) => a
    case Failure(_) => t
  }

  /**
  * Returns this Validation if it is a Success or returns `t` if it is a Failure.
  */
  def orElse[EE >: E, AA >: A](t: => Validation[EE, AA]): Validation[EE, AA] = this match {
    case s @ Success(_) => s
    case Failure(_) => t
  }

  /**
  * Returns None if this is a Failure or a Some containing the value if this is a Success.
  */
  def asOpt = this match {
    case Success(v) => Some(v)
    case Failure(_) => None
  }

  /**
  * Returns Left containing the errors if this is a Failure or a Right containing the value if this is a Success.
  */
  def asEither = this match {
    case Success(v) => Right(v)
    case Failure(e) => Left(e)
  }

  /**
  * Applies the given  partial function `errManager` if this is a Failure, otherwise returns this if this is a Success.
  */
  def recover[AA >: A](errManager: PartialFunction[Failure[E, A], AA]): Validation[E, AA] = this match {
    case Success(v) => Success(v)
    case e@Failure(_) => if (errManager isDefinedAt e) Success(errManager(e)) else this
  }

  /**
  * Applies the given  function `errManager` if this is a Failure, otherwise returns this if this is a Success.
  */
  def recoverTotal[AA >: A](errManager: Failure[E, A] => AA): AA = this match {
    case Success(v) => v
    case e@Failure(_) => errManager(e)
  }

  // TODO: rename (keepAnd ?)
  def *>[EE >: E, B](o: Validation[EE, B]): Validation[EE, B] = (this, o) match {
    case (Success(_), Success(v)) => Success(v)
    case (Success(_), Failure(e)) => Failure(e)
    case (Failure(e), Success(_)) => Failure(e)
    case (Failure(e1), Failure(e2)) => Failure((e1: Seq[E]) ++ (e2: Seq[EE])) // dafuk??? hy do I need to force types ?
  }

  def fail = FailProjection(this)
  def success = SuccessProjection(this)
}

object Validation {

  // this method should be defined for any instance of Monad
  def sequence[E, A](vs: Seq[Validation[E, A]]): Validation[E, Seq[A]] = {
    val reversed = vs.foldLeft[Validation[E, Seq[A]]](Success(Nil)){ (acc, va) =>
      va.flatMap({ a: A =>
        acc.flatMap({ as: Seq[A] =>
          Success(a +: as)
        })
      })
    }
    reversed.flatMap({ as: Seq[A] => Success(as.reverse) })
  }

  import play.api.libs.functional._
  import scala.language.reflectiveCalls

  implicit def applicativeValidation[E] = new Applicative[({type λ[A] = Validation[E, A]})#λ] {

    def pure[A](a: A): Validation[E, A] = Success(a)

    def map[A, B](m: Validation[E, A], f: A => B): Validation[E, B] = m.map(f)

    def apply[A, B](mf: Validation[E, A => B], ma: Validation[E, A]): Validation[E, B] = (mf, ma) match {
      case (Success(f), Success(a)) => Success(f(a))
      case (Failure(e1), Failure(e2)) => Failure.merge(Failure(e1), Failure(e2))
      case (Failure(e), _) => Failure(e)
      case (_, Failure(e)) => Failure(e)
    }
  }

  implicit def alternativeValidation[E](implicit a: Applicative[({type λ[A] = Validation[E, A]})#λ]) = new Alternative[({type λ[A] = Validation[E, A]})#λ] {
    val app = a
    def |[A, B >: A](alt1: Validation[E, A], alt2: Validation[E, B]): Validation[E, B] = (alt1, alt2) match {
      case (Success(v), _) => Success(v)
      case (Failure(e), Success(v)) => Success(v)
      case (Failure(e), _) => Failure(e)
    }
    def empty: Validation[E, Nothing] = Failure(Seq())
  }

   // Helps the compiler a bit
  import play.api.libs.functional.syntax._
  implicit def cba[E] = functionalCanBuildApplicative[({type λ[A] = Validation[E, A]})#λ]
  implicit def ao[E, A](a: Validation[E, A])(implicit alt: Alternative[({type λ[A] = Validation[E, A]})#λ]) =
    toAlternativeOps[({type λ[A] = Validation[E, A]})#λ, A](a)(alt)

  // TODO
  /*
  import play.api.libs.json._
  implicit def pathWrite[I, O](implicit w: Writes[O]) = Writes[(Path[I], O)]{
    case (p, o) => Json.obj("path" -> p.toString, "errors" -> w.writes(o))
  }
  */
}

final case class FailProjection[+E, +A](v: Validation[E, A]) {
  def map[F](f: Seq[E] => Seq[F]): Validation[F, A] = v match {
    case Success(v) => Success(v)
    case Failure(e) => Failure(f(e))
  }
}
final case class SuccessProjection[+E, +A](v: Validation[E, A]) {
  def map[B](f: A => B): Validation[E, B] = v match {
    case Success(v) => Success(f(v))
    case Failure(e) => Failure(e)
  }
}

// Those classes should be final, but we neeed to inherit
// from them for backward compatibility of JsSuccess and JsError
class Success[+E, +A](val value: A) extends Validation[E, A] {
  def get: A = value
  override def toString = s"Success($value)"
  override def hashCode = value.hashCode
  override def equals(o: Any) = {
    if(canEqual(o)) {
      val j = o.asInstanceOf[Success[E, A]]
      this.value == j.value
    }
    else
      false
  }

  def canEqual(o: Any) = o.isInstanceOf[Success[E, A]]
}
class Failure[+E, +A](val errors: Seq[E]) extends Validation[E, A] {
  def get: Nothing = throw new NoSuchElementException("Failure.get")
  override def toString = s"Failure($errors)"
  override def hashCode = errors.hashCode
  override def equals(o: Any) = {
    if(canEqual(o)) {
      val j = o.asInstanceOf[Failure[E, A]]
      this.errors == j.errors
    }
    else
      false
  }
  def canEqual(o: Any) = o.isInstanceOf[Failure[E, A]]
}

object Success {
  def apply[E, A](v: A): Success[E, A] = new Success(v)
  def unapply[E, A](s: Success[E, A]): Option[A] = Some(s.value)
}

object Failure {
  import play.api.libs.functional.Monoid

  def apply[E, A](errors: Seq[E]): Failure[E, A] = new Failure(errors)
  def unapply[E, A](f: Failure[E, A]): Option[Seq[E]] = Some(f.errors)

  def merge[E, A](e1: Failure[E, A], e2: Failure[E, A]): Failure[E, A] = {
    Failure(e1.errors ++ e2.errors)
  }
}