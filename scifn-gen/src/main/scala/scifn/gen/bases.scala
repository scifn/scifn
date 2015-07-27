package scifn.gen

import scala.reflect.api.Universe

object Basis {
  def same[U <: Universe](universe: U)(a: Basis[universe.type], b: Basis[universe.type]): Boolean = (a, b) match {
    case (RequiredBasis(bs1, b1), RequiredBasis(bs2, b2)) =>
      bs1 == bs2 && (b1 equalsStructure b2)
    case (OptionalBasisNoDefault(bs1, b1), OptionalBasisNoDefault(bs2, b2)) =>
      bs1 == bs2 && (b1 equalsStructure b2)
    case (OptionalBasisWithDefault(bs1, b1, d1), OptionalBasisWithDefault(bs2, b2, d2)) =>
      bs1 == bs2 && (b1 equalsStructure b2) && (d1 equalsStructure d2)
    case _ => false
  }
}

/**
 *
 * @tparam U the universe type.
 */
sealed trait Basis[U <: Universe] { // extends Similarity[Basis[U]] {

  /**
   * A description of the basis.
   * @return
   */
  def basisDesc: String

  /**
   * A Tree of a basis. The type the tree should produce is ''scala.Function1''[''A'', ''B''], where ''A''
   * is overall function input type and ''B'' is the (output) type for the basis.
   * @return
   */
  def basis: U#Tree

  /**
   * Whether the basis returns a non ''scala.Option'' value.
   * @return
   */
  def isRequired: Boolean
}

/**
 * @param basisDesc A description of the basis.
 * @param basis A Tree of a basis. The type the tree should produce is ''scala.Function1''[''A'', ''B''],
 *              where ''A'' is overall function input type and ''B'' is the (output) type for the basis.
 * @tparam U the universe type.
 */
case class RequiredBasis[U <: Universe](basisDesc: String, basis: U#Tree) extends Basis[U] {
  override def isRequired = true
}

sealed trait OptionalBasis[U <: Universe] extends Basis[U] {
  def hasDefault: Boolean
}

/**
 * @param basisDesc A description of the basis.
 * @param basis A Tree of a basis. The type the tree should produce is ''scala.Function1''[''A'', Option[''B''] ],
 *              where ''A'' is overall function input type and ''Option''[''B''] is the (output) type for the
 *              basis.
 * @tparam U the universe type.
 */
case class OptionalBasisNoDefault[U <: Universe](basisDesc: String, basis: U#Tree) extends OptionalBasis[U] {
  override def isRequired = false
  override def hasDefault = false
}

/**
 *
 * @param basisDesc A description of the basis.
 * @param basis A Tree of a basis. The type the tree should produce is ''scala.Function1''[''A'', Option[''B''] ],
 *              where ''A'' is overall function input type and ''Option''[''B''] is the (output) type for the
 *              basis.
 * @param default A Tree of type ''B''
 * @tparam U the universe type.
 */
case class OptionalBasisWithDefault[U <: Universe](basisDesc: String, basis: U#Tree, default: U#Tree) extends OptionalBasis[U] {
  override def isRequired = false
  override def hasDefault = true
}
