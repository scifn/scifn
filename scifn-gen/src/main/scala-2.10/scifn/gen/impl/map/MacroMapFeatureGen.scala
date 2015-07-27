package scifn.gen.impl.map

import scifn.gen.{FeatureGen, MacroCompanion, MacroFeatureGen}

import scala.language.experimental.macros
import scala.reflect.macros.Context


object MacroMapFeatureGen extends MacroCompanion[Map[String, Any]] {
  override protected[this] def instance[A <: Map[String, Any] : c.WeakTypeTag](c: Context): MacroFeatureGen[A, c.type, c.universe.type] =
    new MacroFeatureGen[A, c.type, c.universe.type](c)
      with FeatureGen[A, c.universe.type]
      with MapBasisCreator[A, c.universe.type]
}
