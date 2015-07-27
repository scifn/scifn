package scifn.api

import scala.annotation.StaticAnnotation

/**
 * Provides a statically available way of defining imports that will be globally available to
 * all generated functions inside the annotated enclosing class.
 *
 * E.g.
 *
 * {{{
 * // x1 and x2 produce the same value because x2's imports will be deduped and they
 * // were already imported globally to all functions in the class X by including the
 * // import annotation.
 *
 * import scifn.api.imports
 * import scifn.gen.impl.ident.IdentityGeneratedFeautures
 *
 * @imports("scala.math._", "scala.math.BigDecimal.double2bigDecimal")
 * class X extends IdentityGeneratedFeautures {
 *   val x1: Fn[String, scala.math.BigDecimal] = feature("${_}.toDouble")
 *   val x2: Fn[String, scala.math.BigDecimal] =
 *     feature("${_}.toDouble", Vector("scala.math._", "scala.math.BigDecimal.double2bigDecimal"))
 * }
 * }}}
 *
 * '''Note''' that the `imports` annotation needs to appear above the class containing the
 * ''macro application'' (i.e., where the ''macro definition'' is called).
 *
 * In the example below, ''cosine'' is imported only for the `baseClassFn` and that
 * ''absolute value'' and the `sci` alias is only imported for the `subClassFn`.
 * ''Cosine'' is not imported for `subClassFn`.
 *
 * {{{
 * @imports("scala.math.cos")
 * class WithImportsBase extends FnProducer[Any] with IdentityFnProducer[Any] {
 *   val baseClassFn: Fn[String, Double] = fn("cos(${_}.toDouble)")
 * }
 *
 * @imports("scala.math.abs", "scala.collection.{immutable => sci}")
 * class WithImportsSub extends WithImportsBase {
 *   val subClassFn: Fn[String, List[Double]] = fn("sci.List(abs(${_}.toDouble))")
 * }
 * }}}
 * @param values import values.
 */
class imports(values: String*) extends StaticAnnotation
