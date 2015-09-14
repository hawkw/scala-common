package me.hawkweisman.math

import scala.Function._
import scala.language.postfixOps
import scala.reflect.ClassTag
import Numeric.Implicits._

/**
 * A mixin trait providing a linear algebra DSL.
 *
 * Created by hawk on 9/13/15.
 */
trait Linear {
  type Vector[N] = Array[N]
  type Matrix[N] = Vector[Vector[N]]

  def vectorAdd[N : Numeric : ClassTag](a: Vector[N], b: Vector[N]): Vector[N]

  def vectorSub[N : Numeric : ClassTag](a: Vector[N], b: Vector[N]): Vector[N]

  def dotProduct[N : Numeric : ClassTag](a: Vector[N], b: Vector[N]): N

  def matrixAdd[N : Numeric : ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]

  def matrixSub[N : Numeric : ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]

  def crossProduct[N : Numeric : ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]


  implicit class VectorOps[N : Numeric : ClassTag](v: Vector[N])
  {
    require(v.length > 0, "Vectors must have 1 or more elements")

    def +(that: Vector[N]): Vector[N]
      = vectorAdd(v, that)

    def -(that: Vector[N]): Vector[N]
      = vectorSub(v, that)

    def *(that: Vector[N]): N
      = dotProduct(v, that)
  }

  implicit class MatrixOps[N : Numeric : ClassTag](m: Matrix[N])
  {
    require(m.length > 0, "Matrices must have 1 or more columns")
    require(m(0).length > 0, "Matrices must have 1 or more rows")

    def +(that: Matrix[N]): Matrix[N]
      = matrixAdd(m, that)

    def -(that: Matrix[N]): Matrix[N]
      = matrixSub(m, that)

    def *(that: Matrix[N]): Matrix[N]
      = crossProduct(m, that)
  }
}

trait SequentialAlgebra
extends Linear {

  @inline
  private[this] def zap[N: Numeric : ClassTag]
                       (a: Matrix[N], b: Matrix[N])
                       (f: (N, N) ⇒ N): Matrix[N]
    = a zip b map { case ((r1: Array[N], r2: Array[N])) ⇒
        r1 zip r2 map tupled (f)
      }

  override def vectorAdd[N : Numeric : ClassTag]
                        (a: Vector[N], b: Vector[N]): Vector[N]
    = { require(a.length == b.length, "Cannot add vectors of unequal length" )
        a zip b map tupled(_ + _)
      }

  override def vectorSub[N : Numeric : ClassTag]
                        (a: Vector[N], b: Vector[N]): Vector[N]
    = { require( a.length == b.length
                , "Cannot subtract of vectors of unequal length" )
        a zip b map tupled(_ - _)
      }

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
    = { require( a.length == b.length && a(0).length == b(0).length
                , "Cannot take cross product of matrices of unequal size")
        for (row ← a) yield for (col ← b transpose)
          yield row * col
      }

}

trait ParallelAlgebra
extends Linear {

  @inline
  private[this] def zap[N: ClassTag](a: Matrix[N], b: Matrix[N])
                       (f: (N, N) ⇒ N): Matrix[N]
    = a.par zip b.par map { case ((u, v)) ⇒
        u.par zip v.par map tupled (f) toArray
      } toArray

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
    = { require ( a.length == b.length && a(0).length == b(0).length
                , "Cannot take cross product of matrices of unequal size" )
        (for ( row ← a.par )
          yield for ( col ← b.transpose )
            yield row * col) toArray
      }

}
