package scifn

/**
 * Provides a series of extensions from regular functions including diagnostics of
 * problems encountered during function application.
 *
 * Functions of arity 0 ... 20 are defined because there are two additional parameters
 * (f, desc) necessary for the case classes and case classes have an arity restriction
 * of at most 22 in scala 2.10.  Because 2.10 is supported, this is a restriction.
 */
package object func
