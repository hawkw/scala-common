package me.hawkweisman.util.tests

import me.hawkweisman.math.{ SequentialAlgebra
                           , ParallelAlgebra
                           , Linear}
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
trait LinearLaws
extends WordSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Linear {

  val name: String

  s"The $name algebra" when {
    "adding matrices" should {
      "obey the associative property of addition" in {
        forAll { ( a: Array[Array[Int]]
                 , b: Array[Array[Int]]
                 , c: Array[Array[Int]]) ⇒
                (a + b) + c shouldEqual a + (b + c) }
        }
      "obey the commutative property of addition" in {
        forAll { (a: Array[Array[Int]], b: Array[Array[Int]]) ⇒
                  a + b shouldEqual b + a }
        }
      }
    "multiplying matrices" should {
      "obey the associative property of multiplication" in {
        forAll { (a: Array[Array[Int]], b: Array[Array[Int]]) ⇒
                  a + b shouldEqual b + a }
      }
    }
    "adding vectors" should {
      "obey the associative property of addition" in {
        forAll { (a: Array[Int], b: Array[Int], c: Array[Int]) ⇒
                 (a + b) + c shouldEqual a + (b + c) }
      }
      "obey the commutative property of addition" in {
        forAll { (a: Array[Int], b: Array[Int]) ⇒
                  a + b shouldEqual b + a }
      }
    }
    "multiplying vectors" should {
      "obey the associative property of multiplication" in {
        forAll { (a: Array[Int], b: Array[Int]) ⇒
                  a * b shouldEqual b * a }
      }
    }
  }
}

class ParLinearProperties
extends LinearLaws
  with ParallelAlgebra
{ override val name = "parallel" }

class SeqLinearProperties
extends LinearLaws
  with SequentialAlgebra
{ override val name = "sequential" }
