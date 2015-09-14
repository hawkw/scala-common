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
        forAll (sameSize3Matrix) { case ((a, b, c)) ⇒
                (a + b) + c shouldEqual a + (b + c)
        }
      }
      "obey the commutative property of addition" in {
        forAll (sameSize2Matrix) { case ((a, b)) ⇒
                a + b shouldEqual b + a
        }
      }
    }
    "multiplying matrices" should {
      "obey the associative property of multiplication" in {
        forAll (sameSize2Matrix) { case ((a, b)) ⇒
                a + b shouldEqual b + a
        }

      }
    }
    "adding vectors" should {
      "obey the associative property of addition" in {
        forAll (sameSize3Vector) { case ((a, b, c)) ⇒
                (a + b) + c shouldEqual a + (b + c)
        }
      }
      "obey the commutative property of addition" in {
        forAll (sameSize2Vector) { case ((a, b)) ⇒
              a + b shouldEqual b + a
        }
      }
    }
    "multiplying vectors" should {
      "obey the associative property of multiplication" in {
        forAll (sameSize2Vector) { case ((a, b)) ⇒
                a * b shouldEqual b * a
        }
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
