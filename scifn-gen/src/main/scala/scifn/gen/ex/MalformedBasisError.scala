package scifn.gen.ex

import scala.util.parsing.input.Position

case class MalformedBasisError(errorPosition: Position, message: String)
extends RuntimeException(MalformedBasisError.errorMessage(errorPosition, message))

object MalformedBasisError {
  def errorMessage(errorPosition: Position, message: String): String = {
    // 2 b/c ': ' after message.
    val tab = Iterator.fill(message.length + 2)(" ").mkString
    val errInd = errorPosition.longString.replaceFirst("\n", s"\n$tab")
    s"$message: $errInd"
  }
}
