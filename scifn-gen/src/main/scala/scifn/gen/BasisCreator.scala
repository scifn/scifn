package scifn.gen

import scifn.gen.ex.MalformedBasisError

import scala.reflect.api.Universe

/**
 *
 */
trait BasisCreator[A, U <: Universe] { self: FeatureGen[A, U] with FeatureGenReflectEnv[U] =>

  /**
   * Creates a basis or returns a [[MalformedBasisError]].  Bases are the basic building
   * blocks upon which more complicated functions are built.  Each parameter in a basisSpec
   * is backed by a basis.  For instance: in the example
   * {{{
   * val desc: String = "${one} + ${two}"
   * }}}
   *
   * will create two bases.
   * @param basisDesc the basis description
   * @param default a representation of a default value that will be turned into an actual default value.
   * @return
   */
  def basis(basisDesc: String, default: Option[String]): Either[MalformedBasisError, Basis[universe.type]]
}
