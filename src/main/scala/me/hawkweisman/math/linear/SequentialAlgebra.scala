package me.hawkweisman.math
package linear

import scala.Function._
import scala.reflect.ClassTag
import scala.language.postfixOps

import Numeric.Implicits._
import Fractional.Implicits._

/**
 * Sequential linear algebra implementation.
 *
 * This offers better performance than [[ParallelAlgebra]] for very small
 * vectors/matricies (like 2 or 3 element vectors, or 2x2 or 2x3 matrices).
 * However, the parallel implementation is much faster for very large vectors
 * and matrices.
 *
 * A rule of thumb: use [[SequentialAlgebra]] if you're doing graphics, and
 * [[ParallelAlgebra]] if you're doing physics.
 *
 * @author Hawk Weisman
 */
trait SequentialAlgebra
extends Linear {

  @inline
  private[this] def zap[N: Numeric : ClassTag]
                       (a: Matrix[N], b: Matrix[N])
                       (f: (N, N) ⇒ N): Matrix[N]
    = for { (u, v) ← a zip b } yield u zip v map tupled (f)

  override def vectorAdd[N : Numeric : ClassTag]
                        (a: Vector[N], b: Vector[N]): Vector[N]
    = { require(a.length == b.length, "Cannot add vectors of unequal length" )
        a zip b map tupled(_ + _)
      }

  override def vectorSub[N: Numeric: ClassTag]
                        (a: Vector[N], b: Vector[N]): Vector[N]
    = { require( a.length == b.length
                , "Cannot subtract of vectors of unequal length" )
        a zip b map tupled(_ - _)
      }

  override def vectorScalarAdd[N: Numeric: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v map (_ + s)

  override def vectorScalarSub[N: Numeric: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v map (_ - s)

  override def vectorScalarMul[N: Numeric: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v map (_ * s)

  override def vectorScalarDiv[N: Fractional: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v map (_ / s)

  override def vectorMatrixMul[N: Numeric: ClassTag]
                              (v: Vector[N], m: Matrix[N]): Matrix[N]
    = { // currently, this is special cased because
        // unevenly sized vectors isn't done yet
        require(v.length == m(0).length)
        m zip v map tupled { (row, s) ⇒ row ^* s }
      }

  override def matrixScalarAdd[N: Numeric: ClassTag]
                              (m: Matrix[N], s: N): Matrix[N]
    = m map (_ map  (_ + s) )

  override def matrixScalarSub[N: Numeric: ClassTag]
                              (m: Matrix[N], s: N): Matrix[N]
    = m map (_ map  (_ - s) )

  override def matrixScalarMul[N: Numeric: ClassTag]
                              (m: Matrix[N], s: N): Matrix[N]
    = m map (_ map  (_ * s) )

  override def matrixScalarDiv[N: Fractional: ClassTag]
                              (m: Matrix[N], s: N): Matrix[N]
    = m map (_ map  (_ / s) )

  override def dotProduct[N : Numeric : ClassTag]
                         (a: Vector[N], b: Vector[N]): N
    = { require( a.length == b.length
               , "Cannot take dot product of vectors of unequal length" )
        a zip b map tupled (_ * _) sum
      }

  override def matrixAdd[N : Numeric : ClassTag]
                        (a: Matrix[N], b: Matrix[N]): Matrix[N]
    = { require(a.length == b.length, "Cannot add matrices of unequal size")
        zap(a, b)(_ + _)
      }

  override def matrixSub[N : Numeric : ClassTag]
                        (a: Matrix[N], b: Matrix[N]): Matrix[N]
    = { require( a.length == b.length && a(0).length == b(0).length
               , "Cannot subtract matrices of unequal size" )
        zap(a, b)(_ - _)
      }

  override def crossProduct[N : Numeric : ClassTag]
                           (a: Matrix[N], b: Matrix[N]): Matrix[N]
    = { require( a.length == b(0).length
               , "X-cardinality of matrix A must equal y-cardinality of B." )
        for (row ← a)
        yield for (col ← b transpose)
              yield row * col
      }

}
