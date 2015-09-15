package me.hawkweisman.util.tests

import me.hawkweisman.math.{ SequentialAlgebra, ParallelAlgebra, Linear}
import org.scalameter.api._

import scala.util.Random

/**
 * Performance tests comparing the parallel and sequential linear algebra
 * implementations.
 *
 * Created by hawk on 9/14/15.
 */
abstract class LinearBenchmark(val name: String)
extends PerformanceTest.OfflineRegressionReport
  with Linear {

  val sizes = Gen.range("size")(10,1000,50)

  def intMatrix(n: Int): Matrix[Int]
    = Array.ofDim[Int](n * n)
           .map(_ ⇒ Random.nextInt())
           .grouped(n)
           .toArray

  def intVector(n: Int): Vector[Int]
    = Array.ofDim[Int](n)
           .map(_ ⇒ Random.nextInt())

  val intMatrices: Gen[(Matrix[Int], Matrix[Int])]
    = for { n ← sizes }
      yield (intMatrix(n), intMatrix(n))

  val intVectors: Gen[(Vector[Int], Vector[Int])]
    = for { n ← sizes }
      yield (intVector(n), intVector(n))

  performance of s"$name vector algebra" in {
    measure method "vector-vector addition" in {
      using(intVectors) in { case ((u, v)) ⇒ u + v }
    }

    measure method "vector-vector subtraction" in {
      using(intVectors) in { case ((u, v)) ⇒ u - v }
    }

    measure method "vector-vector multiplication" in {
      using(intVectors) in { case ((u, v)) ⇒ u * v }
    }

  }

  performance of s"$name matrix algebra" in {
    measure method "matrix-matrix addition" in {
      using(intMatrices) in { case ((m, n)) ⇒ m + n }
    }

    measure method "matrix-matrix subtraction" in {
      using(intMatrices) in { case ((m, n)) ⇒ m - n }
    }

    measure method "matrix-matrix multiplication" in {
      using(intMatrices) in { case ((m, n)) ⇒ m * n }
    }
  }
}

object SeqLinearBench
  extends LinearBenchmark("sequential")
  with SequentialAlgebra

object ParLinearBench
extends LinearBenchmark("parallel")
  with ParallelAlgebra


