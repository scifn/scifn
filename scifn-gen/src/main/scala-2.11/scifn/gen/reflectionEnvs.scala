package scifn.gen

import scifn.api.imports
import scifn.func.Fn

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.api.Universe
import scala.reflect.macros.blackbox
import scala.reflect.runtime.{currentMirror, universe => ru}
import scala.reflect.{macros => srm}
import scala.tools.reflect.ToolBox
import scala.util.Try

/**
 * A base trait for reflection environment-specific traits to be mixed in with [[FeatureGen]].  The
 * two environments are runtime reflection and compile-time reflection (macros).  This trait provides
 * a `universe` variable and two important functions:
 *
- `parse`: transforms a String to a corresponding `Tree` associated with the underlying `universe`.
 - `freshTermName`: provides a hygienic term name.  See: [[http://docs.scala-lang.org/overviews/quasiquotes/hygiene.html Scala Quasiquote Hygiene page]]
 *
 * @tparam U type of Universe.
 */
sealed trait FeatureGenReflectEnv[U <: Universe] {

  /**
   * A `scala.reflect.api.Universe`
   */
  protected[this] val universe: U

  /**
   * Transform a String into a Tree associated with `universe`.
   * @param code code to turn into a `Tree`.
   * @return a `Tree` representing the code.
   */
  protected[this] def parse(code: String): universe.Tree

  /**
   * Create a new hygienic term name.
   * This uses the method outlined in:
   * [[http://docs.scala-lang.org/overviews/quasiquotes/hygiene.html Scala Quasiquote Hygiene page]]
   * @param prefix prefix of the term name to be produced
   * @return a new term name prefixed by the designated prefix and with a unique numbered suffix
   */
  protected[this] final def freshTermName(prefix: String): universe.TermName =
    universe.internal.reificationSupport.freshTermName(prefix)
}

/**
 * Provides a mixin to [[FeatureGen]] for runtime reflection.  In scala 2.11, this is not only
 * threadsafe, but optimized for use with parallel collections.  For instance, using an
 * implementation such as the ''Identity Fn Producer'', we can write code like:
 *
 * {{{
 * import scifn.func.Fn
 * import scifn.gen.impl.ident.RuntimeIdentityFeatureGen
 * import scifn.gen.FeatureGen.Implicits.runtimeWeakTypeTag
 *
 * class X {
 *   def transform(ss: Seq[String]): Seq[Fn[String, Double] = {
 *     val fg = RuntimeIdentityFeatureGen[String]
 *     // Turn into a parallel collection and get automatic speed up.
 *     ss.par.map(s => fg.compile[Double](s).get)
 *   }
 * }}}
 * @tparam A the domain of the functions being produced.
 */
trait RuntimeFeatureGen[A] extends FeatureGenReflectEnv[ru.type] { self: FeatureGen[A, ru.type] =>
  override protected[this] final val universe: ru.type = ru
  import universe.{Liftable, Tree, WeakTypeTag}

  /**
   * A toolbox responsible for the `parse` and `compile` methods.  This is thread-local
   * because it will automatically boost performance when mapping over parallel functors.
   */
  private[this] final val toolbox = new ThreadLocal[ToolBox[ru.type]] {
    override protected[this] final def initialValue() = currentMirror.mkToolBox()
  }

  /**
   * Transform a String into a Tree associated with `universe`.
   * @param code code to turn into a `Tree`.
   * @return a `Tree` representing the code.
   */
  protected[this] final def parse(code: String): Tree = toolbox.get.parse(code)

  /**
   * Attempt to compile a `Tree` and transform it into an `Fn[A, B]`.
   * @param tree the `Tree` to transform to an `Fn` instance.
   * @tparam B the codomain of the function.
   * @tparam F the type of function produced.  This needs to be supplied because the underlying
   *           scala compilation function in the reflection library is untyped so we need to have
   *           a type to which the result is casted.
   * @return an Fn instance.  This is a function with additional capabilities.
   */
  final def compile[B: WeakTypeTag, F <: Fn[A, B]: WeakTypeTag](tree: Tree): Try[F] =
    Try { toolbox.get.eval(tree).asInstanceOf[F] }

  /**
   * Compile a function with a provided default.
   * @param desc a description of the function to be synthesized.
   * @param default the default value if no value could be returned by the synthesized function.
   * @param imports imports to pass to the function being synthesized.
   * @param bwtt a weak type tag for the output type of the synthesized function.
   * @param lft a `Liftable`, responsible for transforming the default into a `Tree`.
   * @tparam B the codomain of the synthesized function
   * @return an Fn instance.  This is a function with additional capabilities.
   */
  final def compileFnWithDefault[B](desc: String, default: B, imports: Vector[String] = Vector.empty)(implicit bwtt: WeakTypeTag[B], lft: Liftable[B]): Try[Fn[A, B]] = {
    val d = lft(default)
    compile[B, Fn[A, B]](fn(desc, Option(d), imports)(bwtt))
  }

  /**
   * Compile a function without a default.
   * @param desc a description of the function to be synthesized.
   * @param imports imports to pass to the function being synthesized.
   * @param bwtt a weak type tag for the output type of the synthesized function.
   * @tparam B the codomain of the synthesized function
   * @return an Fn instance.  This is a function with additional capabilities.
   */
  final def compileFn[B](desc: String, imports: Vector[String] = Vector.empty)(implicit bwtt: WeakTypeTag[B]): Try[Fn[A, B]] =
    compile[B, Fn[A, B]](fn(desc, None, imports)(bwtt))
}

/**
 * The abstract base class with which implementations of macro-based feature gen are mixed in.  For an
 * example of how to use this, see code for [[scifn.gen.impl.ident.MacroIdentityFeatureGen]].
 *
 * @param c a blackbox macro context.
 * @tparam U  This should be c.universe.type in the instantiation.
 */
abstract class MacroFeatureGen[A, C <: blackbox.Context, U <: srm.Universe](val c: C)(implicit val awtt: U#WeakTypeTag[A])
extends FeatureGenReflectEnv[U] { self: FeatureGen[A, U] with FeatureGenReflectEnv[U] =>

  override protected[this] val universe: U = c.universe.asInstanceOf[U]

  import universe._

  /**
   * Transform a String into a Tree associated with `universe`.
   * @param code code to turn into a `Tree`.
   * @return a `Tree` representing the code.
   */
  protected[this] final def parse(code: String): Tree = c.parse(code).asInstanceOf[Tree]

  /**
   * Get the class that has the macro application.  Because the ''macro definitions'' have a
   * visibility level of `protected[this]`, the class with the ''macro application'' is expected
   * to be either the same class that is mixed in with both [[scifn.api.FnProducer]] and the
   * corresponding trait that specifies the ''macro definitions'' or the ''macro application''
   * is in a derived class of the class with the ''macro definitions''.
   * @return
   */
  private[this] def definingClassSymbol = {
    import c.universe.Expr
    c.prefix match { case Expr(_this) => _this.tpe.typeSymbol }
  }

  /**
   * Get the imports associated with the class containing the ''macro application''.  Get the
   * imports from the `imports` annotation above the class containing the ''macro application''.
   * @return a list of global imports.
   */
  private[this] def importsInClassWithMacroApplication: List[String] = {
    val impType = typeOf[imports]
    definingClassSymbol.annotations.view.map(_.tree).collectFirst {
      case a if a.tpe == impType =>
        // Drop the first child because it is the annotation declaration.
        // We know they are strings because 'imports' only has strings.
        a.children.tail.map { case q"${s: String}" => s }
    } getOrElse Nil
  }

  /**
   * Get the imports for the current function.  This includes both global imports associated
   * with the class containing the ''macro application'' as well as any local imports provided
   * in the ''macro application''.
   * @param localImports expression of a varargs list of local imports.
   * @return a Vector of all imports.  The Vector created by joining global imports and local
   *         imports and then calling distinct on the combined list.
   */
  private[this] def retrieveImports(localImports: c.Expr[String]*): Vector[String] = {
    val localImp = localImports.map { i =>
      val q"${s: String}" = i.tree
      s
    }.toVector
    (importsInClassWithMacroApplication ++: localImp).distinct
  }

  /**
   * Macro implementation for syntheisizing a function with no local imports and no default.
   * @param desc a function description
   * @tparam B the synthesized function codomain
   * @return an Expr of a function
   */
  def fn[B: WeakTypeTag](desc: c.Expr[String]): c.Expr[Fn[A, B]] = {
    val q"${s: String}" = desc.tree
    val imp = retrieveImports()
    c.Expr[Fn[A, B]](fn[B](s, None, imp).asInstanceOf[c.Tree])
  }

  /**
   * Macro implementation for syntheisizing a function with local imports and a default.
   * @param desc a function description
   * @param default a default the synthesized function returns when it cannot otherwise return a value.
   * @param imports imports to add for just this synthesized function
   * @tparam B the synthesized function codomain
   * @return an Expr of a function
   */
  def fnWithDefaultAndImports[B: WeakTypeTag](desc: c.Expr[String], default: c.Expr[B], imports: c.Expr[String]*): c.Expr[Fn[A, B]] = {
    val q"${s: String}" = desc.tree
    val imp = retrieveImports(imports:_*)
    c.Expr[Fn[A, B]](fn[B](s, Option(default.tree.asInstanceOf[universe.Tree]), imp).asInstanceOf[c.Tree])
  }

  /**
   * Macro implementation for syntheisizing a function with local imports but no default.
   * @param desc a function description
   * @param imports imports to add for just this synthesized function
   * @tparam B the synthesized function codomain
   * @return an Expr of a function
   */
  def fnWithImports[B: WeakTypeTag](desc: c.Expr[String], imports: c.Expr[String]*): c.Expr[Fn[A, B]] = {
    val q"${s: String}" = desc.tree
    val imp = retrieveImports(imports:_*)
    c.Expr[Fn[A, B]](fn[B](s, None, imp).asInstanceOf[c.Tree])
  }
}

/**
 * Companion objects containing scifn macros can extend this trait to avoid writing boilerplate.
 * Developers creating a new [[scifn.api.FnProducer]] implementation can extend [[MacroCompanion]]
 * so that they just have to implement the ''instance'' method.
 * @tparam D the domain lower bound.
 */
trait MacroCompanion[D] {

  /**
   * Top-level ''macro implementation'' that creates a function with a default value and local imports.
   * This delegates to `instance`'s `fnWithDefaultAndImports` method.
   * @param c a macro context.
   * @param desc a description of the function to produce.
   * @param default a default value.
   * @param imports local imports.
   * @tparam A domain of the function being materialized.
   * @tparam B codomain of the function being materialized.
   * @return an Expr of a function
   */
  final def fnWithDefaultAndImports[A <: D: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(desc: c.Expr[String], default: c.Expr[B], imports: c.Expr[String]*): c.Expr[Fn[A, B]] =
    instance[A](c).fnWithDefaultAndImports[B](desc, default, imports:_*)

  /**
   * Top-level ''macro implementation'' that creates a function with function specific imports.
   * This delegates to `instance`'s `fnWithImports` method.
   * @param c a macro context.
   * @param desc a description of the function to produce.
   * @param imports local imports.
   * @tparam A domain of the function being materialized.
   * @tparam B codomain of the function being materialized.
   * @return an Expr of a function
   */
  final def fnWithImports[A <: D: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(desc: c.Expr[String], imports: c.Expr[String]*): c.Expr[Fn[A, B]] =
    instance[A](c).fnWithImports[B](desc, imports:_*)

  /**
   * Top-level ''macro implementation'' that creates a function.
   * This delegates to `instance`'s `fn` method.
   * @param c a macro context.
   * @param desc a description of the function to produce.
   * @tparam A domain of the function being materialized.
   * @tparam B codomain of the function being materialized.
   * @return an Expr of a function
   */
  final def fn[A <: D: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(desc: c.Expr[String]): c.Expr[Fn[A, B]] =
    instance[A](c).fn[B](desc)

  /**
   * Create a macro-based feature generation instance of the appropriate type.  If the FeatureGen for
   * input type is appropriately modularized into a based trait to be shared across runtime and
   * compile time reflection environments, like `MyFeatureGen` below, then ''instance'' can be
   * written as so:
   *
   * {{{
   * trait MyBasisCreator[A, U <: Universe] extends BasisCreator[A, U] {
   *   self: FeatureGen[A, U] with FeatureGenReflectEnv[U] =>
   *   def basis(basisDesc: String, default: Option[String]): Either[MalformedBasisError, Basis[universe.type]] = {
   *     // [ implementation here ]
   *   }
   * }
   *
   * object MyMacroFeatureGen extends MacroCompanion[Any] {
   *   override protected[this] def instance[A : c.WeakTypeTag](c: blackbox.Context) =
   *     new MacroFeatureGen[A, c.type, c.universe.type](c)
   *       with FeatureGen[A, c.universe.type]
   *       with MyBasisCreator[A, c.universe.type]
   * }
   * }}}
   * @param c macro context
   * @param awtt WeakTypeTag to provide type information about the domain of the function to be produced.
   * @tparam A The domain of the function to be returned
   * @return a macro-based feature generator instance
   */
  protected[this] def instance[A <: D](c: blackbox.Context)(implicit awtt: c.WeakTypeTag[A]): MacroFeatureGen[A, c.type, c.universe.type]
}
