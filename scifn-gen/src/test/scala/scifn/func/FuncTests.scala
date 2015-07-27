package scifn.func

import org.scalatest._

class FuncTests extends FlatSpec with Matchers {
  "An Fn1" should "throw an exception when the basis has an empty desc" in {
    an [IllegalArgumentException] should be thrownBy {
      Fn(Fn((_:String).toDouble, ""))((_, _) => "Don't Care. Construction will fail.", "Desc doesn't matter.")
    }
  }
}
