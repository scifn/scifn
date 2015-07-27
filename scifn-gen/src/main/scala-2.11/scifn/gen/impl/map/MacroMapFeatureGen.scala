package scifn.gen.impl.map

import scifn.gen.{FeatureGen, MacroCompanion, MacroFeatureGen}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox


object MacroMapFeatureGen extends MacroCompanion[Map[String, Any]] {
  override protected[this] def instance[A <: Map[String, Any] : c.WeakTypeTag](c: blackbox.Context) =
    new MacroFeatureGen[A, c.type, c.universe.type](c)
      with FeatureGen[A, c.universe.type]
      with MapBasisCreator[A, c.universe.type]
}
