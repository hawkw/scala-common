package me.hawkweisman.util.tests

import me.hawkweisman.math.{ SequentialAlgebra
                           , ParallelAlgebra
                           , Linear
                           }
import org.scalatest.{ WordSpec
                     , Matchers
                     }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._

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
        forAll { (m: Array[Array[Int]], r: Int, s: Int) ⇒
          whenever(m.nonEmpty && m(0).nonEmpty) {
            (r * s) *^ m shouldEqual r *^ (s *^ m)
          }
        }
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
        forAll { (v: Array[Int], s: Int) ⇒
          whenever (v nonEmpty) {
            v ^+ s shouldEqual s +^ v
          }
        }
      }
    }
    "multiplying vectors and scalars" should {
      "obey the associative property of multiplication" in {
        forAll { (v: Array[Int], s: Int, r: Int) ⇒
          whenever(v nonEmpty) {
            r *^ (s *^ v) shouldEqual (r * s) *^ v
          }
        }
      }
      "obey the distributive property of multiplication" in {
        forAll { (v: Array[Int], s: Int, r: Int) ⇒
          whenever (v nonEmpty) {
            (r + s) *^ v shouldEqual (r *^ v) + (s *^ v)
          }
        }
      }
    }
  }
}

trait LinearCorrectness
extends WordSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Linear
  with LinearGenerators {

  def name: String

  s"The $name algebra" when {
    "adding matrices" should {
      "evaluate correctly for a known-good example" in {
        val m = Array ( Array(0, 1, 2)
                      , Array(9, 8, 7) )
        val n = Array ( Array(6, 5, 4)
                      , Array(3, 4, 5) )
        var o = Array ( Array(6,  6,  6)
                      , Array(12, 12, 12) )
        n + m shouldEqual o
      }
      "provide the same result as an iterative algorithm" in {
        forAll (sameSize2Matrix) { case ((m: Matrix[Int], n: Matrix[Int])) ⇒
          var o: Array[Array[Int]] = Array ofDim[Int](m.length, m(0).length)

          for { i ← m.indices
                j ← m(0).indices
              } {
                o(i)(j) = m(i)(j) + n(i)(j)
              }
          m + n shouldEqual o
        }
      }
    }
    "subtracting matrices" should {
      "provide the same result as an iterative algorithm" in {
        forAll (sameSize2Matrix) { case ((m: Matrix[Int], n: Matrix[Int])) ⇒
          var o: Array[Array[Int]] = Array ofDim[Int](m.length, m(0).length)

          for { i ← m.indices
                j ←  m(0).indices
          } {
            o(i)(j) = m(i)(j) - n(i)(j)
          }
          m - n shouldEqual o
        }
      }
    }
    "subtracting matrices and scalars" should {
      "provide the same result as an iterative algorithm" in {
        forAll ((singleIntMatrix, "m"), (arbitrary[Int], "s")) {
         (m: Matrix[Int], s: Int) ⇒
            var o: Array[Array[Int]] = Array ofDim[Int](m.length, m(0).length)

            for {i ← m.indices
                 j ← m(0).indices
            } {
              o(i)(j) = m(i)(j) - s
            }
            m ^- s shouldEqual o
        }
      }
    }
    "adding matrices and scalars" should {
      "provide the same result as an iterative algorithm" in {
        forAll ((singleIntMatrix, "m"), (arbitrary[Int], "s")) {
          (m: Matrix[Int], s: Int) ⇒
            var o: Array[Array[Int]] = Array ofDim[Int](m.length, m(0).length)

            for {i ← m.indices
                 j ← m(0).indices
            } {
              o(i)(j) = m(i)(j) + s
            }
            m ^- s shouldEqual o
        }
      }
    }
    "multiplying matrices and scalars" should {
      "provide the same result as an iterative algorithm" in {

        forAll ((singleIntMatrix, "m"), (arbitrary[Int], "s")) {
          (m: Matrix[Int], s: Int) ⇒
            var o: Array[Array[Int]] = Array ofDim[Int](m.length, m(0).length)

            for {i ← m.indices
                 j ← m(0).indices
            } {
              o(i)(j) = m(i)(j) * s
            }
            m ^* s shouldEqual o
        }
      }
    }
    "adding vectors" should {
      "provide the same result as an iterative algorithm" in {
        forAll (sameSize2Vector) { case ((v: Vector[Int], u: Vector[Int])) ⇒
          var w: Vector[Int] = Array ofDim[Int] v.length

          for { i ← v.indices
          } {
            w(i) = v(i) + u(i)
          }
          u + v shouldEqual w
        }
      }
    }
    "subtracting vectors" should {
      "provide the same result as an iterative algorithm" in {
        forAll (sameSize2Vector) { case ((v: Vector[Int], u: Vector[Int])) ⇒
          var w: Vector[Int] = Array ofDim[Int] v.length

          for { i ← v.indices
          } {
            w(i) = v(i) - u(i)
          }
          u - v shouldEqual w
        }
      }
    }
    "subtracting vectors and scalars" should {
      "provide the same result as an iterative algorithm" in {
        forAll ((singleIntVector, "v"), (arbitrary[Int], "s")) {
          (v: Vector[Int], s: Int) ⇒
            var u: Vector[Int] = Array ofDim[Int] v.length

            for { i ← u.indices } {
              u(i) = v(i) - s
            }
            v ^- s shouldEqual u
        }
      }
    }
    "adding vectors and scalars" should {
      "provide the same result as an iterative algorithm" in {
        forAll ((singleIntVector, "v"), (arbitrary[Int], "s")) {
          (v: Vector[Int], s: Int) ⇒
            var u: Vector[Int] = Array ofDim[Int] v.length

            for { i ← u.indices } {
              u(i) = v(i) + s
            }
            v ^+ s shouldEqual u
        }
      }
    }
    "multiplying vectors and scalars" should {
      "provide the same result as an iterative algorithm" in {
        forAll ((singleIntVector, "v"), (arbitrary[Int], "s")) {
          (v: Vector[Int], s: Int) ⇒
            var u: Vector[Int] = Array ofDim[Int] v.length

            for { i ← u.indices } {
              u(i) = v(i) * s
            }
            v ^* s shouldEqual u
        }
      }
    }
  }
}


class ParLinearSpec
extends LinearLaws("parallel")
  with LinearCorrectness
  with ParallelAlgebra

class SeqLinearSpec
extends LinearLaws("sequential")
  with LinearCorrectness
  with SequentialAlgebra
