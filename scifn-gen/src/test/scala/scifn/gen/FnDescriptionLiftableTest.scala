package scifn.gen

import scifn.gen.FeatureGen.Implicits.runtimeWeakTypeTag // This is important!
import scifn.gen.impl.map.RuntimeMapFeatureGen
import scifn.gen.impl.ident.RuntimeIdentityFeatureGen
import org.scalatest._
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.language.existentials

class FnDescriptionLiftableTest extends FlatSpec with Matchers {

  /**
   * This is the only difference necessary to get runtime reflection to work in scala 2.10.
   */
  private[this] implicit val scala210_hack_not_necessary_in_scala211 = runtimeWeakTypeTag[Double]

  val tb = currentMirror.mkToolBox()

  "Generated code" should "be as expected with no imports or default and all required bases" in {
    val fnDescriptionLiftable = RuntimeIdentityFeatureGen[String].fnDescriptionLiftable[Double]
    val desc = "${_}.toDouble"
    val s = FnDescription[universe.type](desc, None, Vector.empty)
    val slCode = normalizeCode(show(fnDescriptionLiftable(s)))
    val expected = """(_root_.scifn.func.ReportingFn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(((x: String) => x), "_"))(((_ro, _x) => _ro(_x).toDouble), "${_}.toDouble")): _root_.scifn.func.Fn[String, Double])"""
    slCode.trim should be (expected.trim)
  }

  it should "be as expected with imports but no default and all required bases" in {
    val fnDescriptionLiftable = RuntimeIdentityFeatureGen[String].fnDescriptionLiftable[Double]
    val desc = "${_}.toDouble"
    val s = FnDescription[universe.type](desc, None, Vector("scala.math._"))
    val slCode = normalizeCode(show(fnDescriptionLiftable(s)))
    val expected =
      """
        |(_root_.scifn.func.ReportingFn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(((x: String) => x), "_"))(((_ro, _x) => {
        |  import _root_.scala.math._;
        |  _ro(_x).toDouble
        |}), "${_}.toDouble")): _root_.scifn.func.Fn[String, Double])
      """.stripMargin
    slCode.trim should be (expected.trim)
  }

  it should "be as expected with default but no imports and all required bases" in {
    val fnDescriptionLiftable = RuntimeIdentityFeatureGen[String].fnDescriptionLiftable[Double]
    val desc = "${_}.toDouble"
    val s = FnDescription[universe.type](desc, Option(tb.parse("\"\"")), Vector.empty)
    val slCode = normalizeCode(show(fnDescriptionLiftable(s)))
    val expected = """(_root_.scifn.func.ReportingFn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(((x: String) => x), "_"))(((_ro, _x) => _ro(_x).toDouble), "${_}.toDouble")): _root_.scifn.func.Fn[String, Double])"""
    slCode.trim should be (expected.trim)
  }

  it should "be as expected with imports, default and all required bases" in {
    val fnDescriptionLiftable = RuntimeIdentityFeatureGen[String].fnDescriptionLiftable[Double]
    val desc = "${_}.toDouble"
    val s = FnDescription[universe.type](desc, Option(tb.parse("\"\"")), Vector("scala.math._"))
    val slCode = normalizeCode(show(fnDescriptionLiftable(s)))
    val expected =
      """
        |(_root_.scifn.func.ReportingFn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(((x: String) => x), "_"))(((_ro, _x) => {
        |  import _root_.scala.math._;
        |  _ro(_x).toDouble
        |}), "${_}.toDouble")): _root_.scifn.func.Fn[String, Double])
      """.stripMargin
    slCode.trim should be (expected.trim)
  }

  it should "throw an Exception with no imports or default and optional and required bases" in {
    val fnDescriptionLiftable = RuntimeMapFeatureGen[Map[String, Int]].fnDescriptionLiftable[Double]
    val desc = "${one:-1}.toDouble + scala.math.pow(${two}, ${two}) + ${four:-4} + ${four:-4}"
    val s = FnDescription[universe.type](desc, None, Vector.empty)
    an [Exception] should be thrownBy { fnDescriptionLiftable(s) }
  }

  it should "throw an Exception with imports but no default and optional and required bases" in {
    val fnDescriptionLiftable = RuntimeMapFeatureGen[Map[String, Int]].fnDescriptionLiftable[Double]
    val desc = "${one:-1}.toDouble + scala.math.pow(${two}, ${two}) + ${four:-4} + ${four:-4}"
    val s = FnDescription[universe.type](desc, None, Vector("scala.math._"))
    an [Exception] should be thrownBy { fnDescriptionLiftable(s) }
  }

  it should "be as expected with default but no imports and optional and required bases" in {
    val fnDescriptionLiftable = RuntimeMapFeatureGen[Map[String, Int]].fnDescriptionLiftable[Double]
    val desc = "${one:-1}.toDouble + scala.math.pow(${two}, ${two}) + ${four:-4} + ${four:-4}"
    val s = FnDescription[universe.type](desc, Option(tb.parse("0.0")), Vector.empty)
    val slCode = normalizeCode(show(fnDescriptionLiftable(s)))
    val expected =
      """
        |(_root_.scifn.func.ReportingFn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(((m: Map[String,Int]) => m.get("four")), 4, "four"), "four"), _root_.scifn.func.Fn(_root_.scifn.func.Fn(((m: Map[String,Int]) => m.get("one")), 1, "one"), "one"), _root_.scifn.func.Fn(((m: Map[String,Int]) => m.get("two")), "two"))({
        |  val _def = 0.0;
        |  ((_rm, _ro, _o, _x) => {
        |    val _vo = _o(_x);
        |    if (_vo.isEmpty)
        |      _def
        |    else
        |      {
        |        val _vr = _rm(_x);
        |        _ro(_x).toDouble.$plus(scala.math.pow(_vo.get, _vo.get)).$plus(_vr).$plus(_vr)
        |      }
        |  })
        |}, "${one:-1}.toDouble + scala.math.pow(${two}, ${two}) + ${four:-4} + ${four:-4}")): _root_.scifn.func.Fn[Map[String,Int], Double])
      """.stripMargin
    slCode.trim should be (expected.trim)
  }

  it should "be as expected with imports, default and optional and required bases" in {
    val fnDescriptionLiftable = RuntimeMapFeatureGen[Map[String, Int]].fnDescriptionLiftable[Double]
    val desc = "${one:-1}.toDouble + scala.math.pow(${two}, ${two}) + ${four:-4} + ${four:-4}"
    val s = FnDescription[universe.type](desc, Option(tb.parse("0.0")), Vector("scala.math._"))
    val slCode = normalizeCode(show(fnDescriptionLiftable(s)))
    val expected =
      """
        |(_root_.scifn.func.ReportingFn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(_root_.scifn.func.Fn(((m: Map[String,Int]) => m.get("four")), 4, "four"), "four"), _root_.scifn.func.Fn(_root_.scifn.func.Fn(((m: Map[String,Int]) => m.get("one")), 1, "one"), "one"), _root_.scifn.func.Fn(((m: Map[String,Int]) => m.get("two")), "two"))({
        |  val _def = 0.0;
        |  ((_rm, _ro, _o, _x) => {
        |    val _vo = _o(_x);
        |    if (_vo.isEmpty)
        |      _def
        |    else
        |      {
        |        val _vr = _rm(_x);
        |        import _root_.scala.math._;
        |        _ro(_x).toDouble.$plus(scala.math.pow(_vo.get, _vo.get)).$plus(_vr).$plus(_vr)
        |      }
        |  })
        |}, "${one:-1}.toDouble + scala.math.pow(${two}, ${two}) + ${four:-4} + ${four:-4}")): _root_.scifn.func.Fn[Map[String,Int], Double])
      """.stripMargin
    slCode.trim should be (expected.trim)
  }

  private[this] def normalizeCode(code: String) = code.
      replaceAll("""_rm\d+""", "_rm").
      replaceAll("""_ro\d+""", "_ro").
      replaceAll("""_o\d+""", "_o").
      replaceAll("""_vr\d+""", "_vr").
      replaceAll("""_vo\d+""", "_vo").
      replaceAll("""_x\d+""", "_x").
      replaceAll("""_def\d+""", "_def")
}
