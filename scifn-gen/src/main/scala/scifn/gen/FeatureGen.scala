package scifn.gen

import scifn.gen.ex.MalformedBasisError
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.reflect.api.Universe
import scala.util.parsing.input.OffsetPosition
import scala.language.implicitConversions

object FeatureGen {
  private val log = LoggerFactory.getLogger(getClass)

  import scala.reflect.runtime.universe.WeakTypeTag
  object Implicits {
    implicit def runtimeWeakTypeTag[A](implicit t: WeakTypeTag[A]): WeakTypeTag[A] = t
  }

  private object Constants {
    val reqOneFuncPrefix = "_ro"
    val reqMultFuncPrefix = "_rm"
    val optFuncPrefix = "_o"

    val reqValPrefix = "_vr"
    val optValPrefix = "_vo"
    val inputValName = "_x"
    val defaulValtName = "_def"
  }

  private object Regex {
    /**
     * Basic template of a variable:
     *
     *   ${[anything but right brace]+}
     */
    val basisTemplate = """(\$(\{[^\}]+\}|[_a-zA-Z0-9]+))""".r

    /**
     * Template of a variable with no default value:
     *
     *   ${[anything but right brace and colon]+}
     */
    val basisNoDefault = """\$\{([^\}:]+)\}""".r

    val basisNoDefaultNoBraces = """\$([_a-zA-Z0-9]+)""".r

    /**
     * Template of a variable with a default value:
     *
     *   ${[anything but right brace and colon]+:-[anything but right brace]+}
     */
    val basisWithDefault = """\$\{([^\}:]+)(:-([^\}]+))?\}""".r
  }
}

trait FeatureGen[A, U <: Universe] { self: FeatureGenReflectEnv[U] with BasisCreator[A, U] =>
  import universe._

  implicit def awtt: WeakTypeTag[A]

  /**
   *
   * @param imports imports to add to the function kernel
   * @param kernel the description with parameters injected
   * @param input the term name for the input variable.
   * @param rmFuncNames names of basis functions with multiple usages and representing required parameters
   * @param rmValNames names of values containing the corresponding bases values from the rmFuncNames bases.
   * @param oFuncNames names of basis functions with an optional parameter
   * @param oValNames names of values containing the corresponding bases values from the oFuncNames bases.
   */
  private[this] case class FuncInfo(
    imports: Seq[String],
    kernel: Tree,
    input: TermName,
    rmFuncNames: Seq[TermName],
    rmValNames: Seq[TermName],
    oFuncNames: List[TermName] = Nil,
    oValNames: List[TermName] = Nil)


  // TODO: Replace B with B <: Fn[Nothing, Any]
  def fn[B](desc: String, default: Option[Tree], imports: Vector[String])(implicit bwtt: WeakTypeTag[B]): Tree = {
    implicit val fnDescriptionLift = fnDescriptionLiftable[B]
    val s = FnDescription[universe.type](desc, default, imports)
    q"$s"
  }

  // TODO: Replace B with B <: Fn[Nothing, Any]
  /**
   *
   * '''Note''': This function is package private for testing.  It should not be called directly.
   * @param bwtt a weak type tag for the output type of the synthesized function.
   * @tparam B the codomain of the synthesized function
   * @return
   */
  protected[gen] final def fnDescriptionLiftable[B](implicit bwtt: WeakTypeTag[B]): Liftable[FnDescription[universe.type]] = new Liftable[FnDescription[universe.type]] {

    import FeatureGen.Constants._
    import FeatureGen.Regex._
    import FeatureGen.log.{debug, isDebugEnabled}

    def apply(s: FnDescription[universe.type]): Tree = {
      gatherBasisFns(s) match {
        case Left(malformedBasisEx) => throw malformedBasisEx
        case Right(basisFns) =>
          val (req, opt) = partitionBases(basisFns)

          // Deal with required variables that specified a default.
          // Get sets of required and not required
          // Look variables with instances that both specified and didn't specify a default

          if (opt.isEmpty)
            allRequiredBases(s, req)
          else someOptionalBases(s, req, opt)
      }
    }

    final def optToReq(o: OptionalBasisWithDefault[universe.type]) =
      RequiredBasis[universe.type](
        o.basisDesc,
        q"_root_.scifn.func.Fn(${o.basis}, ${o.default}, ${o.basisDesc})"
      )

    final def partitionBases(bases: Map[Basis[universe.type], List[String]]) = {
      @tailrec def h(bss: List[(Basis[universe.type], List[String])], req: Map[RequiredBasis[universe.type], List[String]], opt: Map[OptionalBasis[universe.type], List[String]]): (Map[RequiredBasis[universe.type], List[String]], Map[OptionalBasis[universe.type], List[String]]) = {
        bss match {
          case (r@RequiredBasis(_, _), replacements) :: tail               => h(tail, req + (r -> replacements), opt)
          case (o@OptionalBasisNoDefault(_, _), replacements) :: tail      => h(tail, req, opt + (o -> replacements))
          case (o@OptionalBasisWithDefault(_, _, _), replacements) :: tail => h((optToReq(o), replacements) :: tail, req, opt)
          case Nil                                                         => (req, opt)
        }
      }
      h(bases.toList, Map.empty, Map.empty)
    }

    /**
     * Get a''' ''deduplicated'' '''indexed sequence of attempts to create variable
     * definitions.  The algorithm works as follows:
     *
     * Find strings that look like variables.  Then attempt to determine if they:
     1  have a default value
     1  have NO default value
     1  are malformed variables entirely.
     *
     * If either (1) or (2) holds, attempt to create the variable.  It is possible that
     * the variable might not be able to be created even if the variable if well-formed
     * according to the basic structure because the variable domain might not accept the
     * varSpec passed to it.
     * @param fd a function description from which to gather the variables.
     * @return An indexed sequence of attempts at creating the variables.
     */
    final def gatherBasisFns(fd: FnDescription[universe.type])(implicit awtt: WeakTypeTag[A]) = {
      @tailrec def h(bases: List[String], acc: List[(String, Basis[universe.type])]): Either[MalformedBasisError, List[(String, Basis[universe.type])]] = {
        // Note that there is some code repetition here but if we extracted it to functions,
        // we'd have to use trampolining with scala.util.control.TailCalls._.  This would
        // most likely slow down the code.
        bases match {
          case Nil       => Right(acc.reverse)
          case p :: tail => p match {
            case basisWithDefault(bs, _, d) => basis(bs, Option(d)) match {
              case Right(b)   => h(tail, p -> b :: acc)
              case Left(fail) => Left(fail)
            }
            case basisNoDefault(bs) => basis(bs, None) match {
              case Right(b)   => h(tail, p -> b :: acc)
              case Left(fail) => Left(fail)
            }
            case basisNoDefaultNoBraces(bs) => basis(bs, None) match {
              case Right(b)   => h(tail, p -> b :: acc)
              case Left(fail) => Left(fail)
            }
            case basisTemplate(b) =>
              // TODO: Fill in message ...
              Left(new MalformedBasisError(new OffsetPosition(b, b.indexOf(":") + 1), s"Bad basis: '$b'"))
          }
        }
      }

      consolidate(h(basisTemplate.findAllIn(fd.desc).toList, Nil))
    }

    final def consolidate(e: Either[MalformedBasisError, Traversable[(String, Basis[universe.type])]]) =
      e.right.map(_.foldLeft(Map.empty[Basis[universe.type], List[String]]){ case (m, (s, b)) =>
        m.find{ case(bb, _) => Basis.same(universe)(bb, b) } match {
          case Some((b1, ss)) => m.updated(b1, s :: ss)
          case None           => m + (b -> List(s))
        }
      })

    final def codomainIsOption = bwtt.tpe weak_<:< typeOf[Option[Any]]

    /**
     * '''NOTE''': Functions with output type of Option[X], for some X, are treated differently
     * when they contain bases with optional output types.  Since the function is
     * supposed to return an Option and there is a clear zero or unit for that type,
     * namely, ''None'', we can inject this value if no default is supplied.  This
     * makes sense.  Therefore, we can use the more simplified API (with no default)
     * when reifying the functions in both macros and using runtime reflection.  For
     * instance:
     * {{{
     * val fn = Function[Map[String, String], Option[Double]]("Option(${optional.key}.toDouble)")
     * }}}
     * @param fd a function description
     * @return
     */
    final def getDefault(fd: FnDescription[universe.type]) =
      if (fd.default.nonEmpty)
        fd.default
      else if (codomainIsOption)
        Option(reify { None }.tree)
      else None

    final def paramName(name: TermName) = {
      val et = EmptyTree
      q"val $name = $et"
    }

    final def injectOneSetOfParams(
        desc: String,
        inputName: String,
        names: Seq[TermName],
        replacements: Seq[List[String]],
        replaceFn: (String, String) => String) =
      (desc /: names.zip(replacements)){ case (s, (n, r)) =>
        (s /: r.toSet)((ss, x) => ss.replaceAllLiterally(x, replaceFn(n.decodedName.toString, inputName)))
      }

    final def injectParams(
        desc: String,
        inputName: TermName,
        rmValNames: Seq[TermName],
        rMultReps: Seq[List[String]],
        roFuncNames: Seq[TermName],
        rOneOffReps: Seq[List[String]],
        oValNames: Seq[TermName],
        oReps: Seq[List[String]]) = {
      val inName = inputName.decodedName.toString
      val optInserted = injectOneSetOfParams(desc, inName, oValNames, oReps, (v, _) => s"$v.get")
      val reqMultInserted = injectOneSetOfParams(optInserted, inName, rmValNames, rMultReps, (v, _) => v)
      val allInserted = injectOneSetOfParams(reqMultInserted, inName, roFuncNames, rOneOffReps, (f, x) => s"$f($x)")
      parse(allInserted)
    }

    final def allParams(
        input: TermName,
        rmFuncNames: Seq[TermName],
        roFuncNames: Seq[TermName],
        oFuncNames: Seq[TermName]) =
      (rmFuncNames ++ roFuncNames ++ oFuncNames :+ input) map paramName

    final def allBases(
        rMultiples: Seq[Basis[universe.type]],
        rOneOffs: Seq[Basis[universe.type]],
        opt: Seq[Basis[universe.type]]) =
      (rMultiples ++ rOneOffs ++ opt) map { case b =>
        q"_root_.scifn.func.Fn(${b.basis}, ${b.basisDesc})"
      }

    final def getImports(imports: Seq[String]) =
      imports.map {
        case imp if imp.startsWith("_root_.") => parse(s"import $imp")
        case imp                              => parse(s"import _root_.$imp")
      }.toList

    final def functionKernel(fi: FuncInfo) = {
      val rmValDefs = fi.rmFuncNames.zip(fi.rmValNames).map{ case(f, y) => q"val $y = $f(${fi.input})" }
      if (fi.imports.isEmpty)
        q"..$rmValDefs; ${fi.kernel}"
      else {
        val imp = getImports(fi.imports)
        q"..$rmValDefs; ..$imp; ${fi.kernel}"
      }
    }

    /**
     * Isomorphic to a for comprehension followed by a getOrElse.  This uses nested if-then-else
     * statements for efficiency.  It allocates way less memory and seems to be about 25% faster.
     * See the READMEs for more information.
     *
     * Basic algorithm.
     1. Create a variable containing the optional basis value.
     1. If the option is empty, return the default, else, continue down into another if-then-else.
     1. Inside the final else execute the actual statement (since all data is available).
     * @param default a default to return if the generated function can't produce a value
     * @param fi information about the function to be produced
     * @return
     */
    final def optionalFunctionBody(default: TermName)(fi: FuncInfo) = {
      def ifThenElses(vs: List[(TermName, TermName)]): Tree = vs match {
        case Nil            => functionKernel(fi)
        case (v, f) :: tail =>
          // Recurse here to get the else part since this is a bunch of nested if then statements.
          // The stack depth shouldn't be deep because it's based on the number of unique optional
          // bases, which is bounded by the arity of Fn (which is around 20).
          val elseTree = ifThenElses(tail)
          q"""val $v = $f(${fi.input})
              if ($v.isEmpty)
                $default
              else $elseTree
           """
      }

      ifThenElses(fi.oValNames.zip(fi.oFuncNames))
    }

    final def functionComponents(
        fd: FnDescription[universe.type],
        bodyFunc: FuncInfo => Tree,
        req: Map[RequiredBasis[universe.type], List[String]],
        opt: Map[OptionalBasis[universe.type], List[String]] = Map.empty) = {

      val (rOneOffs, rMultiples)      = req.toVector.partition(_._2.size == 1)
      val (rOneOffBases, rOneOffReps) = rOneOffs.unzip
      val (rMultBases, rMultReps)     = rMultiples.unzip
      val (optBases, optReps)         = opt.toVector.unzip

      val roFuncNames = rOneOffBases.map(_ => freshTermName(reqOneFuncPrefix))
      val rmValNames  = rMultBases.map(_ => freshTermName(reqValPrefix))
      val rmFuncNames = rMultBases.map(_ => freshTermName(reqMultFuncPrefix))
      val oValNames   = optBases.map(_ => freshTermName(optValPrefix)).toList
      val oFuncNames  = optBases.map(_ => freshTermName(optFuncPrefix)).toList
      val inputName   = freshTermName(inputValName)

      val bases        = allBases(rMultBases, rOneOffBases, optBases)
      val params       = allParams(inputName, rmFuncNames, roFuncNames, oFuncNames)
      val injectedSpec = injectParams(fd.desc, inputName, rmValNames, rMultReps,
                                      roFuncNames, rOneOffReps, oValNames, optReps)
      val funcInfo     = FuncInfo(fd.imports, injectedSpec, inputName,
                                  rmFuncNames, rmValNames, oFuncNames, oValNames)
      val body         = bodyFunc(funcInfo)
      (bases, params, body)
    }

    /**
     * Constuct a function that looks like the following:
     *
     * {{{
     * val f = Fn(..bases)(
     *   (_r0, _r1, _a) => {
     *     val _rv0 = _r0(_a)
     *     import x.y.z
     *     _r1(_a) - _vr0
     *   },
     *   "${r1} - ${r0}"
     * )
     * }}}
     * @param fd
     * @param req
     * @return
     */
    final def allRequiredBases(fd: FnDescription[universe.type], req: Map[RequiredBasis[universe.type], List[String]]) = {
      val (bases, params, body) = functionComponents(fd, functionKernel, req)
      val code = q"""_root_.scifn.func.ReportingFn(
                       _root_.scifn.func.Fn(..$bases)(
                         (..$params) => $body,
                         ${fd.desc}
                       )
                     ): _root_.scifn.func.Fn[$awtt, $bwtt]"""

      if (isDebugEnabled)
        debug(s"Creating function with all required bases: '${fd.desc}': ${show(code).replaceAll("\n", raw"\\n")}")

      code
    }


    /**
     * Constuct a function that looks like the following:
     *
     * {{{
     * val f = Fn(..bases)(
     *   {
     *     val _default = -123.456
     *     (_r0, _r1, _o0, _o1, _a) => {
     *       val _ov0 = _o0(_a)
     *       if (_ov0.isEmpty) default
     *       else {
     *         val _ov1 = _o1(_a)
     *         if (_ov1.isEmpty) default
     *         else {
     *           val _rv0 = _r0(_a)
     *           import x.y.z
     *           _r1(_a) + _vo0.get - _vr0 + _vo1.get
     *         }
     *       }
     *     }
     *   },
     *   "${r1} + ${o0} - ${r0} + ${o1}"
     * )
     * }}}
     *
     * '''NOTE''': return types of Option[X] are treated specially.  If no default is provided in desc and
     * the return type is Option[X], then a default of ''None'' will automatically be assumed.  This is not
     * the case for other type constructors with a clear ''unit'' or ''zero'' element.  For instance, for
     * ''Iterable'', we do not assume ''Iterable.empty''.
     * @param fd
     * @param req
     * @param opt
     * @return
     */
    def someOptionalBases(
        fd: FnDescription[universe.type],
        req: Map[RequiredBasis[universe.type], List[String]],
        opt: Map[OptionalBasis[universe.type], List[String]]) =
      getDefault(fd).fold(throw new Exception("Optional bases but no global function default")){ default =>
        val defValName = freshTermName(defaulValtName)
        val optFuncBody = optionalFunctionBody(defValName) _
        val (bases, params, body) = functionComponents(fd, optFuncBody, req, opt)
        val code = q"""_root_.scifn.func.ReportingFn(
                         _root_.scifn.func.Fn(..$bases)(
                           {
                             val $defValName = $default
                             (..$params) => $body
                           },
                           ${fd.desc}
                         )
                       ) : _root_.scifn.func.Fn[$awtt, $bwtt]"""

        if (isDebugEnabled) {
//          val defaultStr = fd.default.map(d => " (default: '" + showCode(d) + "')").getOrElse("")
//          debug(s"Creating function with optional bases: '${fd.desc}'$defaultStr: ${showCode(code).replaceAll("\n", raw"\\n")}")
          val defaultStr = fd.default.map(d => " (default: '" + show(d) + "')").getOrElse("")
          debug(s"Creating function with optional bases: '${fd.desc}'$defaultStr: ${show(code).replaceAll("\n", raw"\\n")}")
        }

        code
      }
    }
}
