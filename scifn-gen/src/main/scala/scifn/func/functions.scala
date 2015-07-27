package scifn.func

import scifn.func.Fn.syntax._

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Success, Try}

// TODO: Look at specialization.

/**
 * Basis functions are building blocks to bigger functions in FnN.
 * @tparam A function domain
 * @tparam B function codomain
 */
sealed trait Basis[-A, B] extends Fn[A, B] { self: Product => }

/**
 * Guaranteed to have at least two fields in the product:
 1   The combiner function
 1   The desc string which describes the function.
 *
 * Following these are the feature extracting functions whose domain is A.
 *
 * '''Note''': The functions were generated with:
 * {{{
 * val numFunc = 20
 * // For case classes.
 * for (i <- 1 to numFunc; vars = 0 until i) {
 *   println(s"""final case class Fn${i}[A, ${vars.map(v => s"F${v}").mkString(", ")}, +B](f: (${vars.map(v => s"A ↦ F${v}").mkString(", ")}, A) => B, desc: String, ${vars.map(v => s"f${v}: A ↦ F${v}").mkString(", ")}) extends Fn[A, B] {
 *              |  def apply(a: A) = f(${vars.map(v => s"f${v}").mkString(", ")}, a)
 *              |}
 *            """.stripMargin
 *   )
 * }
 *
 * // AND ...
 *
 * // For factory functions in Fn object.
 * for (i <- 2 to numFunc; vars = 0 until i) {
 *   println(s"""def apply[A, ${vars.map(v => s"F${v}").mkString(", ")}, B](${vars.map(v => s"f${v}: A ↦ F${v}").mkString(", ")})(f: (${vars.map(v => s"A ↦ F${v}").mkString(", ")}, A) => B, desc: String) = Fn${i}(f, desc, ${vars.map(v => s"f${v}").mkString(", ")})""")
 * }
 * }}}
 * @tparam A function domain
 * @tparam B function codomain Notice: The codomain is not covariant.  This is because the macros cannot
 *                                     infer the type of B when the macro application is assigned to a
 *                                     variable with a type ascription.
 */
@throws[IllegalArgumentException]("if extractor functions are not FnN[_, _] or have empty desc values.")
sealed trait Fn[-A, B] extends (A => B) { self: Product =>

  /**
   * A description of the function.
   */
  def desc: String

  /**
   * The number of basis functions.
   * @return
   */
  def arity: Int = productArity - 2

  /**
   * @return an iterator of basis functions.
   */
  def basisFns: Iterator[Basis[A, Any]] = productIterator.asInstanceOf[Iterator[Basis[A, Any]]].drop(2)

  /**
   * @param a the input value.
   * @return an iterator of values outputted by the basis functions wrapped in a Try.
   */
  def basisVals(a: A): Iterator[Try[Any]] = basisFns.map(v => Try(v(a)))


  // fn: FThenFn, FnThenFn, FnThenF, Fn
  // then: Fn, F

//  override def andThen[C](g: B => C): Fn[A, C] = (this, g) match {
//    case (fn: FThenFn[A, _, B], then: Fn[B, C])  =>
//    case (fn: FThenFn[A, _, B], then: (B => C))  =>
//
//    case (fn: FnThenF[A, _, B], then: Fn[B, C])  =>
//    case (fn: FnThenF[A, _, B], then: (B => C))  =>
//
//    case (fn: FnThenFn[A, _, B], then: Fn[B, C]) =>
//    case (fn: FnThenFn[A, _, B], then: (B => C)) =>
//
//    case (fn: Fn[A, B], then: Fn[B, C])          =>
//    case (fn: Fn[A, B], then: (B => C))          =>


    //    case (fn: FnThenF[A, _, B], then: Fn[B, C])  => FnThenFn(fn.fn, FThenFn(fn.then, then))
//    case (fn: FnThenF[A, _, B], then: (B => C))  => fn.copy(then = fn.then andThen then)
//    case (fn: FnThenFn[A, _, B], then: Fn[B, C]) => fn.copy(then = then andThen g)
//    case (fn: FnThenFn[A, _, B], then: (B => C)) => fn.copy(then = fn.then andThen g)
//    case (fn: Fn[A, B], then: Fn[B, C])          => FnThenFn(fn, then andThen g)
//    case (fn: Fn[A, B], then: (B => C))          => FnThenFn(fn, then andThen g)
//  }

  /**
   * @param a the input value.
   * @return function diagnostics including missing values and error found in the basis function output.
   */
  def diagnostics(a: A): FnDiagnostics[A] = {
    @tailrec def h(missing: List[A ↦ Any],
                   err: List[A ↦ Any],
                   it: Iterator[(A ↦ Any, Try[Any])]): FnDiagnostics[A] = {

      if (!it.hasNext)                FnDiagnostics(missing.reverse, err.reverse)
      else {
        val (v, iv) = it.next()
        if (iv.isFailure)             h(missing, v :: err, it)
        else if (iv == Success(None)) h(v :: missing, err, it)
        else                          h(missing, err, it)
      }
    }
    h(Nil, Nil, basisFns.zip(basisVals(a)))
  }

  require(
    productIterator.drop(2).forall {
      case f: Basis[_, _] if f.desc.trim.nonEmpty => true
      case _ => false
    },
    "basis functions must a Basis instance with a non-empty desc."
  )
}

sealed trait FnWrapper[-A, B, C] extends Fn[A, C] { self: Product =>
  def fn: Fn[A, B]
  override def desc              = fn.desc
  override def arity             = fn.arity
  override def basisFns          = fn.basisFns
  override def basisVals(a: A)   = fn.basisVals(a)
  override def diagnostics(a: A) = fn.diagnostics(a)
}

/**
 *
 * Notice that all factory methods follow the same structure except the one that produces
 * values Fn0.  This does not have an empty first argument list.  Therefore, it must be
 * treated separately when writing macros and reflection-based code.
 *
 * NOTE: Compile errors follow when apply factory methods have any of the following:
 1 default values for desc
 1 For ANY ARITY, Two alternative methods, one with desc and one without.
 *
 * Because of this, there is no default value for desc.
 */
object Fn {

  /**
   * Provides type alias ''↦'' for Basis.
   * '''This is intended for internal use in this package.'''
   */
  private[func] object syntax {

    /**
     * Alias for Basis.  Unicode: 0x21A6.
     * @tparam A function domain
     * @tparam B function codomain
     */
    type ↦[-A, B] = Basis[A, B]
  }

  /**
   * Takes a ''scala.Function1''[''A'', ''B''] and a description (''desc'') and lifts to a Fn[A, B].
   * @param f A regular ''scala.Function1'' to wrap.
   * @param desc a text description of f(a)
   * @tparam A function domain
   * @tparam B function codomain
   * @return a lifted function.
   */
  def apply[A, B](f: A => B, desc: String) = Fn0(f, desc)

  /**
   * Takes a ''scala.Function1''[''A'', Option[''B''] ], a default and a description (''desc'') and lifts to a
   * Fn[A, Option[B] ] then wraps in an OptFn.
   * @param f A regular ''scala.Function1'' to wrap.
   * @param default a default value to return when f produces None.
   * @param desc a text description of f(a)
   * @tparam A function domain
   * @tparam B function codomain
   * @return a lifted function.
   */
  def apply[A, B](f: A => Option[B], default: B, desc: String) = OptBasis(Fn0(f, desc), default)

  /**
   * Produce a function whose natural arity is 1.  This means that the underlying function relies on
   * only one ''simple'' input that is extracted from the possibly ''complex'' input value of type ''A''.
   *
   * This method and all factory (''apply'') methods have two argument lists to help with type inference.
   * For more information, see the example below.
   *
   * {{{
   * import scala.util.Try
   * import scifn.func.Fn
   *
   * // =====  A function that extracts height value in millimeters.  =====
   *
   * // Only returns a value if it is a positive integer.
   * // Notice that we don't need to specify the output type b/c it's inferred.
   * val height_mm = Fn((csv: Array[String]) => Try { Option(csv(0).toInt).filter(_ > 0) } getOrElse None, "h_mm")
   *
   *
   * // =====  A function that extracts height value in centimeters.  =====
   *
   * // Notice that since we lifted the data into the domain of extraction functions
   * // augmented with the original input, we can completely disregard input and output
   * // type in the combiner function.  This is possible due to the help the type inference
   * // gets from including the basis functions in the first parameter list.  The resulting
   * // combiner function becomes less cluttered with type info so the meaning comes through
   * // more easily.
   *
   * // REPL gives type: height_cm: scifn.func.Fn1[Array[String],Option[Int],Option[Long]]
   * val height_cm = Fn(height_mm)((ht, a) => ht(a).map(h => (h / 10d).round), "(h_mm / 10d).round")
   * }}}
   *
   * @param f0 a function that pulls data from a value of type ''A'' to produce a value of type ''F0''.
   * @param f a function that given a function f0 and a datum from which to extract data,
   *          produces an output of type ''B''.
   * @param desc a text description of the function ''f''(''f0''(''a'')).
   * @tparam A input type of the overall function.
   * @tparam F0 the intermediate value type.
   * @tparam B output type of the overall function.
   * @return A function
   */
  def apply[A, F0, B](f0: A ↦ F0)(f: (A ↦ F0, A) => B, desc: String): Fn1[A, F0, B] = Fn1(f, desc, f0)
  def apply[A, F0, F1, B](f0: A ↦ F0, f1: A ↦ F1)(f: (A ↦ F0, A ↦ F1, A) => B, desc: String) = Fn2(f, desc, f0, f1)
  def apply[A, F0, F1, F2, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A) => B, desc: String) = Fn3(f, desc, f0, f1, f2)
  def apply[A, F0, F1, F2, F3, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A) => B, desc: String) = Fn4(f, desc, f0, f1, f2, f3)
  def apply[A, F0, F1, F2, F3, F4, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A) => B, desc: String) = Fn5(f, desc, f0, f1, f2, f3, f4)
  def apply[A, F0, F1, F2, F3, F4, F5, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A) => B, desc: String) = Fn6(f, desc, f0, f1, f2, f3, f4, f5)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A) => B, desc: String) = Fn7(f, desc, f0, f1, f2, f3, f4, f5, f6)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A) => B, desc: String) = Fn8(f, desc, f0, f1, f2, f3, f4, f5, f6, f7)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A) => B, desc: String) = Fn9(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A) => B, desc: String) = Fn10(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A) => B, desc: String) = Fn11(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A) => B, desc: String) = Fn12(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A) => B, desc: String) = Fn13(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A) => B, desc: String) = Fn14(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A) => B, desc: String) = Fn15(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A) => B, desc: String) = Fn16(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A) => B, desc: String) = Fn17(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16, f17: A ↦ F17)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A ↦ F17, A) => B, desc: String) = Fn18(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16, f17: A ↦ F17, f18: A ↦ F18)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A ↦ F17, A ↦ F18, A) => B, desc: String) = Fn19(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
  def apply[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, B](f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16, f17: A ↦ F17, f18: A ↦ F18, f19: A ↦ F19)(f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A ↦ F17, A ↦ F18, A ↦ F19, A) => B, desc: String) = Fn20(f, desc, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)
}

/**
 * Wrap a function
 * @param fn A function ([[Fn]]) that returns an Option of a value.
 * @param default a default
 * @tparam A function domain
 * @tparam B function codomain
 */
final case class OptBasis[-A, B](fn: Fn0[A, Option[B]], default: B)
  extends Basis[A, B]
  with FnWrapper[A, Option[B], B] {

  def apply(a: A) = fn(a) getOrElse default
}

final case class Fn0[-A, B](f: A => B, desc: String) extends Basis[A, B] {
  def apply(a: A) = f(a)
}

final case class Fn1[A, F0, B](f: (A ↦ F0, A) => B, desc: String, f0: A ↦ F0) extends Fn[A, B] {
  def apply(a: A) = f(f0, a)
}

final case class Fn2[A, F0, F1, B](f: (A ↦ F0, A ↦ F1, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, a)
}

final case class Fn3[A, F0, F1, F2, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, a)
}

final case class Fn4[A, F0, F1, F2, F3, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, a)
}

final case class Fn5[A, F0, F1, F2, F3, F4, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, a)
}

final case class Fn6[A, F0, F1, F2, F3, F4, F5, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, a)
}

final case class Fn7[A, F0, F1, F2, F3, F4, F5, F6, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, a)
}

final case class Fn8[A, F0, F1, F2, F3, F4, F5, F6, F7, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, a)
}

final case class Fn9[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, a)
}

final case class Fn10[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, a)
}

final case class Fn11[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, a)
}

final case class Fn12[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, a)
}

final case class Fn13[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, a)
}

final case class Fn14[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, a)
}

final case class Fn15[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, a)
}

final case class Fn16[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, a)
}

final case class Fn17[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, a)
}

final case class Fn18[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A ↦ F17, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16, f17: A ↦ F17) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, a)
}

final case class Fn19[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A ↦ F17, A ↦ F18, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16, f17: A ↦ F17, f18: A ↦ F18) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, a)
}

final case class Fn20[A, F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, B](f: (A ↦ F0, A ↦ F1, A ↦ F2, A ↦ F3, A ↦ F4, A ↦ F5, A ↦ F6, A ↦ F7, A ↦ F8, A ↦ F9, A ↦ F10, A ↦ F11, A ↦ F12, A ↦ F13, A ↦ F14, A ↦ F15, A ↦ F16, A ↦ F17, A ↦ F18, A ↦ F19, A) => B, desc: String, f0: A ↦ F0, f1: A ↦ F1, f2: A ↦ F2, f3: A ↦ F3, f4: A ↦ F4, f5: A ↦ F5, f6: A ↦ F6, f7: A ↦ F7, f8: A ↦ F8, f9: A ↦ F9, f10: A ↦ F10, f11: A ↦ F11, f12: A ↦ F12, f13: A ↦ F13, f14: A ↦ F14, f15: A ↦ F15, f16: A ↦ F16, f17: A ↦ F17, f18: A ↦ F18, f19: A ↦ F19) extends Fn[A, B] {
  def apply(a: A) = f(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, a)
}

final case class ReportingFn[-A, B](fn: Fn[A, B]) extends FnWrapper[A, B, B] {
  override def apply(a: A): B =
    try {
      fn(a)
    }
    catch {
      case e: FnDiagnosticsException => throw e
      case e: Throwable              => throw new FnDiagnosticsException(fn.diagnostics(a), e)
    }
}
