package scifn.bench.runtime

import org.scalameter.Key._
import org.scalameter._
import org.scalameter.reporting.ChartReporter.ChartFactory
import org.scalameter.reporting.{ChartReporter, DsvReporter, HtmlReporter, RegressionReporter}
import scifn.gen.FeatureGen.Implicits.runtimeWeakTypeTag
import scifn.gen.impl.ident.RuntimeIdentityFeatureGen

import scala.collection.parallel.immutable.ParVector
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

object RuntimeGenBenchmark extends PerformanceTest.OnlineRegressionReport {

  /**
   * This is the only difference necessary to get runtime reflection to work in scala 2.10.
   */
  private[this] implicit val scala210_hack_not_necessary_in_scala211 = runtimeWeakTypeTag[Double]

  private[this] val processors: Int = Runtime.getRuntime.availableProcessors

  val nFunctions: Gen[Int] = Gen.exponential("# features")(12, 24, 2)
  val flat    = for (n <- nFunctions) yield (1 to n).map { _ + " + ${_}.toDouble" }.toVector
  val flatPar = for (f <- flat)       yield ParVector(f:_*)
  val per1    = for (n <- nFunctions) yield grouped(n, processors)

  val global = Context (
    exec.maxWarmupRuns -> 5,
    exec.benchRuns -> 10,          // 10 benchmark runs
    exec.independentSamples -> 2,  // 2 JVM instances
    exec.requireGC -> true,
    exec.jvmflags -> "-server -Xms3g -Xmx3g -XX:MaxPermSize=256m -XX:+UseConcMarkSweepGC -XX:ReservedCodeCacheSize=64m"
  )

  @transient implicit lazy val exCtx = scala.concurrent.ExecutionContext.Implicits.global

  override def reporter: Reporter = Reporter.Composite(
    new RegressionReporter(
      RegressionReporter.Tester.OverlapIntervals(),
      RegressionReporter.Historian.ExponentialBackoff()),
    new DsvReporter(','),
    HtmlReporter(true),
    ChartReporter(ChartFactory.XYLine())
  )

  performance of "RuntimeGen" in {
    performance of "1 instance per core (parallel)" in {
      using(per1) config global in { p =>
        val futures = p map { strs =>
          Future {
            val f = RuntimeIdentityFeatureGen[String]
            strs.aggregate(0d)((s, x) => s + f.compileFn[Double](x).get("0"), _ + _)
          }
        }
        Await.result(Future.fold(futures)(0d)(_ + _), 300 seconds)
      }
    }

    performance of "1 instance per generation (parallel)" in {
      using(flatPar) config global in { f =>
        f.aggregate(0d)((s, x) => s + RuntimeIdentityFeatureGen[String].compileFn[Double](x).get("0"), _ + _)
      }
    }

    performance of "1 instance (parallel)" in {
      using(flatPar) config global in { f =>
        val gen = RuntimeIdentityFeatureGen[String]
        f.aggregate(0d)((s, x) => s + gen.compileFn[Double](x).get("0"), _ + _)
      }
    }

    performance of "1 instance" in {
      using(flat) config global in { f =>
        val gen = RuntimeIdentityFeatureGen[String]
        f.aggregate(0d)((s, x) => s + gen.compileFn[Double](x).get("0"), _ + _)
      }
    }

    performance of "1 instance per generation" in {
      using(flat) config global in { f =>
        f.aggregate(0d)((s, x) => s + RuntimeIdentityFeatureGen[String].compileFn[Double](x).get("0"), _ + _)
      }
    }
  }

  private[this] def grouped(functions: Int, nProcessors: Int): Vector[Vector[String]] = {
    val np = functions / nProcessors
    (1 to nProcessors).map{i => (1 to np).map{j => (np * i + j) + " + ${_}.toDouble"}.toVector }.toVector
  }
}
