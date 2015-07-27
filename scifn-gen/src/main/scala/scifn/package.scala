/**
 * ''' ''SciFn'' ''' provides, among other things, a toolkit for lifting string-based expressions into functions.
 * Descriptions look like plain [[http://scala-lang.org Scala]] expressions where function parameters
 * use the well-known format adapted from shell scripts.  Parameter substitutions looks like one of the
 * following three:
 *
 -  `$parameter`, where ''parameter'' adheres to the regular expression "`[_a-zA-Z0-9]+`".
 -  `${parameter}` where ''parameter'' is a non-empty sequence of characters not including "`:`" or "`}`".
 -  `${parameter:-default}` where ''parameter'' is as above and ''default'' is a non-empty sequence of characters not including "`}`".
 *
 * For instance, a description for ''elliptical eccentricity'' may look like:
 *
 * {{{
 * "sqrt(1 - pow(${semiMinor} / ${semiMajor}, 2))"
 * }}}
 *
 * This indicates that a function whould be created such that for every input, some parameter values
 * `semiMinor` and `semiMajor` would be extracted from the input and injected into the expression to
 * yield the function's result.
 *
 * Note in the ''elliptical eccentricity'' example above that we are making use of two mathematical
 * helper functions: `sqrt` and `pow`.  Implementations of these functions are defined in the
 * `scala.math` package.  ''' ''SciFn'' ''' provides a mechanism for importing anything that exists
 * on the classpath, so the ''' ''SciFn'' ''' language (''post hoc'') can be modified in almost arbitrary
 * ways.  Since [[http://docs.scala-lang.org/tutorials/tour/implicit-parameters.html implicits]] and
 * [[http://docs.scala-lang.org/overviews/core/implicit-classes.html implicit classes]] are so powerful,
 * the description language ''' ''SciFn'' ''' can be tailored to almost anything the function author
 * desires.
 *
 * How parameters are handled in the parsing of descriptions depends on the subclass of
 * [[scifn.api.FnProducer]] in macro-based generation and on the subclass of
 * [[scifn.gen.RuntimeFeatureGen]] for runtime-based generation.
 */
package object scifn {}
