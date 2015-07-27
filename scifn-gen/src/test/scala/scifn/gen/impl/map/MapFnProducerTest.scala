package scifn.gen.impl.map

import scifn.api.{FnProducer}
import scifn.func.Fn
// import scifn.gen.FeatureGen.Implicits.runtimeWeakTypeTag // This is important! See below.
import org.scalatest._

/**
 *
 */
class MapFnProducerTest
  extends FlatSpec
  with Matchers
  with FnProducer[Map[String, Int]]
  with MapFnProducer[Map[String, Int]] {

  "A function with optional params and optional output created via macro" should "compile with neither default nor imports." in {
    val f: Fn[Map[String, Int], Option[Double]] = fn("Option(${one}.toDouble + ${two}.toLong)")
    f(Map("one" -> 1, "two" -> 2)).nonEmpty should be (true)
  }

  it should "compile with None default but no imports." in {
    val f: Fn[Map[String, Int], Option[Double]] = fn("Option(${one}.toDouble + ${two}.toLong)", None)
    f(Map("one" -> 1, "two" -> 2)).nonEmpty should be (true)
  }

  it should "compile with Option.empty default but no imports." in {
    val f: Fn[Map[String, Int], Option[Double]] = fn("Option(${one}.toDouble + ${two}.toLong)", Option.empty)
    f(Map("one" -> 1, "two" -> 2)).nonEmpty should be (true)
  }

  "A function with optional params and param default but no function default" should "compile." in {
    val f: Fn[Map[String, Int], Double] = fn("${one:-123}.toDouble")
    f(Map.empty) should be (123d)
  }

  "A function with optional params and List output created via macro"  should "compile with a default and imports." in {
    val f: Fn[Map[String, Int], List[Double]] =
      fn(
        "sci.List(pow(${one}.toDouble, abs(${two}.toLong)))",
        List.empty,
        "scala.math._",
        "scala.collection.{immutable => sci}"
      )
    f(Map("one" -> 2, "two" -> -3)).head should be (8.0)
  }
}
