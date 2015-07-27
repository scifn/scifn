package scifn.gen.impl.map

import scifn.api.FnProducer
import scifn.func.Fn
import scifn.gen._
import scifn.gen.ex.MalformedBasisError

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.api.Universe
import scala.reflect.runtime.{universe => ru}

trait MapBasisCreator[A <: Map[String, Any], U <: Universe] extends BasisCreator[A, U] {
  self: FeatureGen[A, U] with FeatureGenReflectEnv[U] =>

  /**
   * Create a basis.
   * @param basisDesc the basis description
   * @param default
   * @return
   */
  def basis(basisDesc: String, default: Option[String]): Either[MalformedBasisError, OptionalBasis[universe.type]] = {
    import universe.Quasiquote
    default match {
      case Some(d) => Right(OptionalBasisWithDefault[universe.type](basisDesc, q"(m: $awtt) => m.get($basisDesc)", parse(d)))
      case None    => Right(OptionalBasisNoDefault[universe.type](basisDesc, q"(m: $awtt) => m.get($basisDesc)"))
    }
  }
}

case class RuntimeMapFeatureGen[A <: Map[String, Any]](implicit val awtt: ru.WeakTypeTag[A])
   extends MapBasisCreator[A, ru.type]
      with FeatureGen[A, ru.type]
      with RuntimeFeatureGen[A]

/**
 * Since "term macros cannot override abstract methods", we require the mixin type to be
 * FeatureProducer because this trait cannot extend GeneratedFeatures.  This will at
 * least ensure that all of the same methods will be available.
 *
 */
trait MapFnProducer[D <: Map[String, Any]] { self: FnProducer[D] =>
  protected[this] final implicit def fn[A <: D, B](desc: String): Fn[A, B] =
    macro MacroMapFeatureGen.fn[A, B]

  protected[this] final def fn[A <: D, B](desc: String, default: B, imports: String*): Fn[A, B] =
    macro MacroMapFeatureGen.fnWithDefaultAndImports[A, B]

  protected[this] final def fnImp[A <: D, B](desc: String, imports: String*): Fn[A, B] =
    macro MacroMapFeatureGen.fnWithImports[A, B]
}
