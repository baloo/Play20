package play.api.data.validation


case class Rule[I, O](m: Mapping[(Path[I], Seq[ValidationError]), I, O]) {
  def validate(data: I): VA[I, O] = m(data)
}


object Rule {
	import play.api.libs.functional._

	implicit def applicativeRule[I] = new Applicative[({type f[O] = Rule[I, O]})#f] {
    override def pure[A](a: A): Rule[I, A] =
      Rule(Mapping{ (_: I) => Success(a) })

    override def map[A, B](m: Rule[I, A], f: A => B): Rule[I, B] =
      Rule(Mapping{ d => m.m(d).map(f) })

    override def apply[A, B](mf: Rule[I, A => B], ma: Rule[I, A]): Rule[I, B] =
      Rule(Mapping{ d =>
        val a = ma.validate(d)
        val f = mf.validate(d)
        val res = (f *> a).flatMap(x => f.map(_(x)))
        res
      })
  }

  implicit def functorRule[I] = new Functor[({type f[O] = Rule[I, O]})#f] {
    def fmap[A, B](m: Rule[I, A], f: A => B): Rule[I, B] = applicativeRule[I].map(m, f)
  }

  // Helps the compiler a bit
  import play.api.libs.functional.syntax._
  implicit def cba[I] = functionalCanBuildApplicative[({type f[O] = Rule[I, O]})#f]
  implicit def fbo[I, O] = toFunctionalBuilderOps[({type f[O] = Rule[I, O]})#f, O] _
}
