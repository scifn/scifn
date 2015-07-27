package scifn.gen

import scala.reflect.api.Universe

/**
 * A function description.
 *
 * Some examples of desc:
 *
 * {{{
 * val desc1 = "abs($var1)"
 * val desc  = "abs(${var._1})"    // but abs($var._1) is NOT OK.
 *
 * // .toInt isn't part of the parameter that becomes a basis.  It is applied
 * // to the return value of $var.
 * val desc3 = "abs($var.toInt)"
 * }}}
 * @param desc A description of a function to be produced.  This is Scala expression
 *             where function parameters are represented within delimiters `${` and `}`.
 *             If a parameter name is a valid Java identifier, the braces in the
 *             delimiter can be omitted.  The way that the description is interpreted
 *             is based on the type of FeatureGen instance interpreting it.
 * @param default If the parameter is optional in the resultant function domain, then
 *                a default is required, otherwise, None may be provided.
 * @param imports A series of imports that can be supplied to the resulting function.
 *                These imports can be used in
 * @tparam U A universe type supplied by the proper reflection environment.  This is
 *           supplied by [[RuntimeFeatureGen]] or [[MacroFeatureGen]].
 */
case class FnDescription[U <: Universe](desc: String, default: Option[U#Tree], imports: Vector[String])
