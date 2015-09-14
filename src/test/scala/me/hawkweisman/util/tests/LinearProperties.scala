package me.hawkweisman.util.tests

import me.hawkweisman.math.{ SequentialAlgebra
                           , ParallelAlgebra
                           , Linear
                           }
import org.scalatest.{ WordSpec
                     , Matchers
                     }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
 * ScalaCheck proofs testing whether linear algebra properties hold on
 * my [[Linear.Matrix Matrix]] and [[Linear.Vector Vector]] implementation.
 *
 * Created by hawk on 9/13/15.
 */
abstract class LinearLaws(val name: String)
extends WordSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Linear
  with LinearGenerators {

  s"The $name algebra" when {
    "adding matrices" should {
      "obey the associative property of addition" in {
        forAll (sameSize3Matrix) { case ((m, n, o)) ⇒
                (m + n) + o shouldEqual m + (n + o)
        }
      }
      "obey the commutative property of addition" in {
        forAll (sameSize2Matrix) { case ((m, n)) ⇒
                m + n shouldEqual n + m
        }
      }
    }
    "multiplying matrices" should {
      "obey the associative property of multiplication" in {
        forAll (sameSize2Matrix) { case ((m, n)) ⇒
                m + n shouldEqual n + m
        }
      }
    }
    "multiplying matrices and scalars" should {
      "obey the associative property of multiplication" in {
        forAll { (m: Matrix[Int], r: Int, s: Int) ⇒
          (r * s) * m shouldEqual r * (s * m) }
      }
    }

    "adding vectors" should {
      "obey the associative property of addition" in {
        forAll (sameSize3Vector) { case ((u, v, w)) ⇒
                (u + v) + w shouldEqual u + (v + w)
        }
      }
      "obey the commutative property of addition" in {
        forAll (sameSize2Vector) { case ((u, v)) ⇒
              u + v shouldEqual v + u
        }
      }
    }
    "multiplying vectors" should {
      "obey the associative property of multiplication" in {
        forAll (sameSize2Vector) { case ((u, v)) ⇒
                u * v shouldEqual v * u
        }
      }
    }
    "adding vectors and scalars" should {
      "obey the commutative property of addition" in {
        forAll { (v: Vector[Int], s: Int) ⇒ v + s shouldEqual s + v }
      }
    }
    "multiplying vectors and scalars" should {
      "obey the associative property of multiplication" in {
        forAll { (v: Vector[Int], s: Int, r: Int) ⇒
          r * (s * v) shouldEqual (r * s) * v }
      }
      "obey the distributive property of multiplication" in {
        forAll { (v: Vector[Int], s: Int, r: Int) ⇒
          (r + s) * v shouldEqual (r * v) + (s * v) }
      }
    }
  }
}

class ParLinearProperties
extends LinearLaws("parallel")
  with ParallelAlgebra

class SeqLinearProperties
extends LinearLaws("sequential")
  with SequentialAlgebra
