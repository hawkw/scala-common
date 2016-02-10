package me.hawkweisman.math
package linear

import scala.Fractional.Implicits._
import scala.Function._
import scala.Numeric.Implicits._
import scala.language.postfixOps
import scala.reflect.ClassTag

/**
 * Parallel linear algebra implementation.
 *
 * This uses Scala's
 * [[scala.collection.parallel.mutable.ParArray explicitly parallel arrays]] to
 * parallelize vector and matrix operations. This offers much slower
 * performance than [[SequentialAlgebra]] for small vectors, due to the added
 * cost of transforming the arrays into parallel arrays. However, this will
 * likely offer much better performance than [[SequentialAlgebra]] for
 * very large vectors and matrices.
 *
 * A general rule of thumb: use [[ParallelAlgebra]] if you're doing physics or
 * stats, and [[SequentialAlgebra]] if you're doing 2D or 3D transformation
 * matrices.
 *
 * @author Hawk Weisman
 */
trait ParallelAlgebra
extends Linear {

  @inline
  private[this] def zap[N: ClassTag](a: Matrix[N], b: Matrix[N])
                       (f: (N, N) ⇒ N): Matrix[N]
    = (for{ (u, v) ← a.par zip b.par }
       yield u.par zip v.par map tupled (f) toArray) toArray

  override def vectorAdd[N : Numeric : ClassTag]
                        (a: Vector[N], b: Vector[N]): Vector[N]
    = { require(a.length == b.length, "Cannot add vectors of unequal length" )
        a.par zip b.par map tupled (_ + _) toArray
      }

  override def vectorSub[N : Numeric : ClassTag]
                        (a: Vector[N], b: Vector[N]): Vector[N]
    = { require( a.length == b.length
               , "Cannot subtract vectors of unequal length" )
        require(a.length > 0, "Vectors must have nonzero length")
         a.par zip b.par map tupled (_ - _ ) toArray
      }

  override def dotProduct[N : Numeric : ClassTag]
                         (a: Vector[N], b: Vector[N]): N
  = { require( a.length == b.length
              , "Cannot take dot product of vectors of unequal length" )
      a.par zip b.par map tupled (_ * _) sum
    }


  override def vectorScalarAdd[N: Numeric: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v.par map (_ + s) toArray

  override def vectorScalarSub[N: Numeric: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v.par map (_ - s) toArray

  override def vectorScalarMul[N: Numeric: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v.par map (_ * s) toArray

  override def vectorScalarDiv[N: Fractional: ClassTag]
                              (v: Vector[N], s: N): Vector[N]
    = v.par map (_ / s) toArray

  override def matrixScalarAdd[N: Numeric: ClassTag]
                              (m: Matrix[N], s: N): Matrix[N]
    = m.par map (_.par map (_ + s) toArray ) toArray

  override def matrixScalarSub[N: Numeric: ClassTag]
                              (m: Matrix[N], s: N): Matrix[N]
    = m.par map (_.par map  (_ - s) toArray ) toArray

  override def matrixScalarMul[N: Numeric: ClassTag]
  (m: Matrix[N], s: N): Matrix[N]
    = m.par map (_.par map (_ * s) toArray ) toArray

  override def matrixScalarDiv[N: Fractional: ClassTag]
                              (m: Matrix[N], s: N): Matrix[N]
    = m.par map (_.par map  (_ / s) toArray ) toArray

  override def matrixAdd[N : Numeric : ClassTag]
                        (a: Matrix[N], b: Matrix[N]): Matrix[N]
    = { require ( a.length == b.length && a(0).length == b(0).length
                , "Cannot add matrices of unequal size" )
        zap(a, b)(_ + _)
      }

  override def matrixSub[N : Numeric : ClassTag]
                        (a: Matrix[N], b: Matrix[N]): Matrix[N]
    = { require ( a.length == b.length && a(0).length == b(0).length
                , "Cannot subtract matrices of unequal size" )
        zap(a, b)(_ - _)
      }

  override def crossProduct[N : Numeric : ClassTag]
                           (a: Matrix[N], b: Matrix[N]): Matrix[N]
    = { require ( a.length == b(0).length
                , "X-cardinality of matrix A must equal y-cardinality of B." )
        (for ( row ← a.par )
          yield for ( col ← b.transpose )
            yield row * col) toArray
      }

  override def vectorMatrixMul[N: Numeric: ClassTag]
                              (v: Vector[N], m: Matrix[N]): Matrix[N]
    = { // currently, this is special cased because
        // unevenly sized vectors isn't done yet
        require(v.length == m(0).length)
        m.par zip v.par map tupled { (row, s) ⇒ row ^* s } toArray
    }

}
