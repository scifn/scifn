package scifn.gen.impl.ident

import scifn.gen.{FeatureGen, MacroCompanion, MacroFeatureGen}

import scala.language.experimental.macros
import scala.reflect.macros.Context

object MacroIdentityFeatureGen extends MacroCompanion[Any] {
  override protected[this] def instance[A : c.WeakTypeTag](c: Context): MacroFeatureGen[A, c.type, c.universe.type] =
    new MacroFeatureGen[A, c.type, c.universe.type](c)
      with FeatureGen[A, c.universe.type]
      with IdentityBasisCreator[A, c.universe.type]
}
