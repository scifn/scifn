package scifn.api

import scifn.func.Fn

/**
 * A trait to be mixed in with a trait containing ''macro definitions''.  This is the best
 * we can hope for because macro definitions cannot extend abstract methods.
 * @tparam D The lower bound of the domain of the feature to be produced.
 */
trait FnProducer[D] {

  /**
   * Given a description, create a corresponding function.
   *
   * The implementation should make the function `implicit`.  That is, it should read:
   *
   * {{{
   * protected[this] final implicit def fn[A <: D, B](desc: String): Fn[A, B] = macro ...
   * }}}
   *
   * This is a work-around for a few issues that arise with using macros.  Placing a concrete
   * method here that uses this `fn` function throws an `java.lang.AbstractMethodError`
   * at runtime.  If we insist that this method is defined as an `implicit` function here,
   * we get
   * {{{
   * [error] /.../package.scala: overloaded macro method fn needs result type
   * [error]   protected[this] final implicit def fn[A <: D, B](desc: String): Fn[A, B] =
   * [error]                                      ^
   * }}}
   * @param desc function description
   * @tparam A function domain
   * @tparam B function codomain
   * @return a function that adheres to the description
   */
  protected[this] def fn[A <: D, B](desc: String): Fn[A, B]

  /**
   * Given a description, default value, and series of imports, create a corresponding function.
   *
   * The implementation should read:
   *
   * {{{
   * protected[this] final def fn[A <: D, B](desc: String, default: B, imports: String*): Fn[A, B] = macro ...
   * }}}
   *
   * @param desc function description
   * @param default a default value that will be returned by the function if it cannot otherwise return a value.
   * @param imports series of imports to be included in the definition of just this function
   * @tparam A function domain
   * @tparam B function codomain
   * @return a function that adheres to the description
   */
  protected[this] def fn[A <: D, B](desc: String, default: B, imports: String*): Fn[A, B]

  /**
   * Given a description, and series of imports, create a corresponding function.
   *
   * The implementation should read:
   *
   * {{{
   * protected[this] final def fnImp[A <: D, B](desc: String, imports: String*): Fn[A, B] = macro ...
   * }}}
   *
   * @param desc function description
   * @param imports series of imports to be included in the definition of just this function
   * @tparam A function domain
   * @tparam B function codomain
   * @return a function that adheres to the description
   */
  protected[this] def fnImp[A <: D, B](desc: String, imports: String*): Fn[A, B]
}
