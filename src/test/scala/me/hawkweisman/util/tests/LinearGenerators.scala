package me.hawkweisman.util.tests

import me.hawkweisman.math.Linear
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose,listOfN}

import scala.reflect.ClassTag


/**
 * Mixin with generators gor making random vectors/matrices for
 * ScalaCheck property tests
 * Created by hawk on 9/14/15.
 */
trait LinearGenerators
extends Linear {

  val MAX_ARRAY_SIZE = 100

  def genMatrix[T: ClassTag](g: Gen[T])(n: Int): Gen[Matrix[T]]
    = listOfN(n*n, g) map ( _.toArray.grouped(n).toArray )

  val intMatrix = genMatrix (arbitrary[Int]) _

  def genVector[T: ClassTag](g: Gen[T])(n: Int): Gen[Vector[T]]
    = listOfN(n, g) map (_.toArray)

  val intVector = genVector (arbitrary[Int]) _

  val sameSize3Matrix = for { n ← choose(2, MAX_ARRAY_SIZE)
                              a ← intMatrix(n)
                              b ← intMatrix(n)
                              c ← intMatrix(n) }
                        yield (a, b, c)

  val sameSize2Matrix = for { n ← choose(2, MAX_ARRAY_SIZE)
                              a ← intMatrix(n)
                              b ← intMatrix(n) }
                        yield (a, b)

  val sameSize3Vector = for { n ← choose(2, MAX_ARRAY_SIZE)
                              a ← intVector(n)
                              b ← intVector(n)
                              c ← intVector(n) }
                        yield (a, b, c)

  val sameSize2Vector = for { n ← choose(2, MAX_ARRAY_SIZE)
                              a ← intVector(n)
                              b ← intVector(n) }
                        yield (a, b)
}
