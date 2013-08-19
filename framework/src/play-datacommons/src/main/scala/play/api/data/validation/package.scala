package play.api.data

/**
 * Contains the validation API used by `Form`.
 *
 * For example, to define a custom constraint:
 * {{{
 *   val negative = Constraint[Int] {
 *     case i if i < 0 => Valid
 *     case _ => Invalid("Must be a negative number.")
 *   }
 * }}}
 */
package object validation {
	@annotation.implicitNotFound("No implicit Mapping found from ${I} to ${O}. Try to define an implicit Mapping[${E}, ${I}, ${O}].")
	type Mapping[E, I, O] = I => Validation[E, O]
	type Constraint[T] = Mapping[ValidationError, T, T]
	type VA[I, O] = Validation[(Path[I], Seq[ValidationError]), O]
}