package scifn.gen.impl.ident

import scifn.gen.{FeatureGen, MacroCompanion, MacroFeatureGen}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox


/**
 *
 */
object MacroIdentityFeatureGen extends MacroCompanion[Any] {
  override protected[this] def instance[A : c.WeakTypeTag](c: blackbox.Context) =
    new MacroFeatureGen[A, c.type, c.universe.type](c)
      with FeatureGen[A, c.universe.type]
      with IdentityBasisCreator[A, c.universe.type]
}
