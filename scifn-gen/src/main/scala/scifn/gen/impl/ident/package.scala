package scifn.gen.impl.ident

import scifn.api.FnProducer
import scifn.func.Fn
import scifn.gen._
import scifn.gen.ex.MalformedBasisError

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.api.Universe
import scala.reflect.runtime.{universe => ru}
import scala.util.parsing.input.OffsetPosition

trait IdentityBasisCreator[A, U <: Universe] extends BasisCreator[A, U] {
  self: FeatureGen[A, U] with FeatureGenReflectEnv[U] =>

  /**
   * Create a basis.
   * @param basisDesc the basis description
   * @param default
   * @return
   */
  def basis(basisDesc: String, default: Option[String]): Either[MalformedBasisError, Basis[universe.type]] = {
    import universe.Quasiquote
    if (basisDesc != "_")
      Left(MalformedBasisError(OffsetPosition(basisDesc, 0), "Should be empty."))
    else Right(RequiredBasis[universe.type](basisDesc, q"(x: $awtt) => x"))
  }
}

case class RuntimeIdentityFeatureGen[A](implicit val awtt: ru.WeakTypeTag[A])
   extends IdentityBasisCreator[A, ru.type]
      with FeatureGen[A, ru.type]
      with RuntimeFeatureGen[A]

/**
 * Since "term macros cannot override abstract methods", we require the mixin type to be
 * FeatureProducer because this trait cannot extend GeneratedFeatures.  This will at
 * least ensure that all of the same methods will be available.
 *
 */
trait IdentityFnProducer[D] { self: FnProducer[D] =>
  protected[this] final implicit def fn[A <: D, B](desc: String): Fn[A, B] =
    macro MacroIdentityFeatureGen.fn[A, B]

  protected[this] final def fn[A <: D, B](desc: String, default: B, imports: String*): Fn[A, B] =
    macro MacroIdentityFeatureGen.fnWithDefaultAndImports[A, B]

  protected[this] final def fnImp[A <: D, B](desc: String, imports: String*): Fn[A, B] =
    macro MacroIdentityFeatureGen.fnWithImports[A, B]
}
