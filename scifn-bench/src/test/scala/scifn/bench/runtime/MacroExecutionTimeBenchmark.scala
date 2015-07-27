package scifn.bench.runtime

import org.scalameter.Key._
import org.scalameter._
import org.scalameter.reporting.ChartReporter.ChartFactory
import org.scalameter.reporting.{ChartReporter, DsvReporter, HtmlReporter, RegressionReporter}
import scifn.api.FnProducer
import scifn.func.Fn
import scifn.gen.impl.map.MapFnProducer

import scala.language.postfixOps

object MacroExecutionTimeBenchmark
  extends PerformanceTest.OnlineRegressionReport
  with FnProducer[Map[String, Int]]
  with MapFnProducer[Map[String, Int]] {


  val emptyMap         = Map.empty[String, Int]
  val size: Gen[Int]   = Gen.exponential("# prediction")(10000, 1000000, 10)
  val ranges           = for (n <- size) yield 1 to n
  val bothItemExamples = for (xs <- ranges) yield xs.map(i => Map("one" -> i, "two" -> (i + 1)))
  val noItemExamples   = for (xs <- ranges) yield xs.map(i => emptyMap)
  val oneItemExamples  = for (xs <- ranges) yield xs.map(i => Map("one" -> i))

  val no   = Gen.tupled(size, noItemExamples)
  val one  = Gen.tupled(size, oneItemExamples)
  val both = Gen.tupled(size, bothItemExamples)

  val global = Context (
    exec.requireGC -> true,
    exec.jvmflags -> "-server -Xms3g -Xmx3g -XX:MaxPermSize=256m -XX:+UseConcMarkSweepGC -XX:ReservedCodeCacheSize=64m"
  )

  override def reporter: Reporter = Reporter.Composite(
    new RegressionReporter(
      RegressionReporter.Tester.OverlapIntervals(),
      RegressionReporter.Historian.ExponentialBackoff()),
    new DsvReporter(','),
    HtmlReporter(true),
    ChartReporter(ChartFactory.XYLine())
  )

  def combine[A, U, V, B](u: A => U, v: A => V)(f: (A => U, A => V, A) => B) = (a: A) => f(u, v, a)

  val monadWithFunctionCalls = combine((_:Map[String, Int]).get("one"), (_:Map[String, Int]).get("two")){(u, v, a) =>
    (for {
      one <- u(a)
      two <- v(a)
     } yield List(math.pow(one.toDouble, math.abs(two.toLong))).head
    ) getOrElse 0.0
  }

  val monad = (m: Map[String, Int]) =>
    (for {
      one <- m.get("one")
      two <- m.get("two")
     } yield List(math.pow(one.toDouble, math.abs(two.toLong))).head
    ) getOrElse 0.0

  val fb: Fn[Map[String, Int], Double] = fn("sci.List(pow(${one}.toDouble, abs(${two}.toLong))).head", 0.0, "scala.collection.{immutable => sci}", "scala.math._")

  performance of "Macro" in {
    performance of "monad-empty map" in {
      using(no) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += monad(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "monad-1 item map" in {
      using(one) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += monad(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "monad-2 item map" in {
      using(both) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += monad(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "monad func-empty map" in {
      using(no) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += monadWithFunctionCalls(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "monad func-1 item map" in {
      using(one) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += monadWithFunctionCalls(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "monad func-2 item map" in {
      using(both) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += monadWithFunctionCalls(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "scifn-empty map" in {
      using(no) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += fb(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "scifn-1 item map" in {
      using(one) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += fb(xs(i))
          i += 1
        }
        s
      }
    }

    performance of "scifn-2 item map" in {
      using(both) config global in { case (n, xs) =>
        var s = 0.0
        var i = 0
        while (i < n) {
          s += fb(xs(i))
          i += 1
        }
        s
      }
    }
  }
}
