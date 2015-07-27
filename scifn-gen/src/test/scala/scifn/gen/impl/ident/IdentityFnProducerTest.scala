package scifn.gen.impl.ident

import scifn.api.{FnProducer, imports}
import scifn.func.Fn
import scifn.gen.FeatureGen.Implicits.runtimeWeakTypeTag // This is important! See below.
import scifn.gen.impl.ident.IdentityFnProducerTest.WithImportsSub
import org.scalatest._

/**
 * Notice that the imports associated with a function defined in a class needs to appear in
 * an annotation for that class.  For instance, ''cosine'' is imported only for the `baseClassFn`
 * and that ''absolute value'' and `sci` alias is only imported for the `subClassFn`.  ''Cosine''
 * is not imported for `subClassFn`.
 */
object IdentityFnProducerTest {
  @imports("scala.math.cos")
  private class WithImportsBase extends FnProducer[Any] with IdentityFnProducer[Any] {
    val baseClassFn: Fn[String, Double] = fn("cos(${_}.toDouble)")
  }

  @imports("scala.math.abs", "scala.collection.{immutable => sci}")
  private class WithImportsSub extends WithImportsBase {
    val subClassFn: Fn[String, List[Double]] = fn("sci.List(abs(${_}.toDouble))")
  }
}

class IdentityFnProducerTest
  extends FlatSpec
  with Matchers
  with FnProducer[Any]
  with IdentityFnProducer[Any] {

  /**
   * This is the only difference necessary to get runtime reflection to work in scala 2.10.
   */
  private[this] implicit val scala210_hack_not_necessary_in_scala211 = runtimeWeakTypeTag[Double]

  @imports (
    "scala.math._",
    "scala.collection.{immutable => sci}"
  )
  private[this] object WithImports extends FnProducer[Any] with IdentityFnProducer[Any] {
    val f: Fn[String, List[Double]] = fn("sci.List(abs(${_}.toDouble))")
  }

  "A function created via macro" should "compile when type is provided by the assignee." in {
    val f: Fn[String, Double] = fn("${_}.toDouble")
    f("123") should be (123d)
  }

  it should "work implicitly with string literals" in {
    val fns: List[Fn[String, Double]] = List("1 + ${_}.toInt", "2 + ${_}.toInt")
    val results = fns.map(f => f("0"))
    results should be ((1 to 2).toList)
  }

  it should "compile when type is provided in the call." in {
    val f = fn[String, Double]("${_}.toDouble")
    f("123") should be (123d)
  }

  it should "compile when type is provided by the assignee inside another type constructor." in {
    val fs: Seq[Fn[String, Double]] = Seq(fn("${_}.toDouble"))
    fs.head("123") should be (123d)
  }

  it should "compile with a default and no imports." in {
    val f = fn[String, Double]("${_}.toDouble", 1.0)
    f("123") should be (123d)
  }

  it should "compile with no default but some imports." in {
    val f: Fn[String, Double] = fnImp(
      "abs(sci.List(${_}.toDouble + 1).head)",
      "_root_.scala.math._",
      "_root_.scala.collection.{immutable => sci}"
    )
    f("123") should be (124d)
  }

  it should "be able to use global imports." in {
    val f = WithImports.f
    val y = f("-1")
    y should be (List(1.0))
  }

  it should "be able to use global imports when macro definitions are mixed into superclass as long as imports annotation defined in the class with the macro application." in {
    val f = new WithImportsSub

    val yb = f.baseClassFn("0")
    yb should be (1.0)

    val ys = f.subClassFn("-1")
    ys should be (List(1.0))
  }

  it should "compile with no braces." in {
    val f: Fn[String, Double] = fn("$_.toDouble")
    f("123") should be (123d)
  }

  it should "operate the same when created with runtime reflection." in {
    // Need to import implicits to avoid having to explicitly provide a WeakTypeTag:
    // import scifn.gen.FeatureGen.Implicits.runtimeWeakTypeTag
    //
    // import scala.reflect.runtime.universe.weakTypeTag
    // val fn = RuntimeIdentityFeatureGen[String].compileFn("${_}.toDouble")(weakTypeTag[Double]).get

    // In scala 2.10, use:
    //   private[this] implicit val scala210_hack_not_necessary_in_scala211 = runtimeWeakTypeTag[Double]
    // This sucks but can't find an easy workaround.
    val fRuntime = RuntimeIdentityFeatureGen[String].compileFn[Double]("${_}.toDouble").get
    val fMacro: Fn[String, Double] = fnImp("${_}.toDouble")
    fMacro("123") should be (fRuntime("123"))
  }
}
