package me.hawkweisman.math

import scala.Function._
import scala.language.postfixOps
import scala.reflect.ClassTag
import Numeric.Implicits._
import Fractional.Implicits._


/**
 * A mixin trait providing a linear algebra DSL.
 *
 * Created by hawk on 9/13/15.
 */
trait Linear {
  type Vector[N] = Array[N]
  type Matrix[N] = Vector[Vector[N]]

  def vectorAdd[N: Numeric: ClassTag](a: Vector[N], b: Vector[N]): Vector[N]
  def vectorSub[N: Numeric: ClassTag](a: Vector[N], b: Vector[N]): Vector[N]

  def vectorScalarAdd[N: Numeric: ClassTag](v: Vector[N], s: N): Vector[N]
  def vectorScalarSub[N: Numeric: ClassTag](v: Vector[N], s: N): Vector[N]
  def vectorScalarDiv[N: Fractional: ClassTag](v: Vector[N], s: N): Vector[N]
  def vectorScalarMul[N: Numeric: ClassTag](v: Vector[N], s: N): Vector[N]

  def dotProduct[N: Numeric: ClassTag](a: Vector[N], b: Vector[N]): N

  def matrixAdd[N: Numeric: ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]
  def matrixSub[N: Numeric: ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]

  def matrixScalarAdd[N: Numeric: ClassTag](m: Matrix[N], s: N): Matrix[N]
  def matrixScalarSub[N: Numeric: ClassTag](m: Matrix[N], s: N): Matrix[N]
  def matrixScalarDiv[N: Fractional: ClassTag](m: Matrix[N], s: N): Matrix[N]
  def matrixScalarMul[N: Numeric: ClassTag](m: Matrix[N], s: N): Matrix[N]

  def crossProduct[N: Numeric: ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]

  implicit class VectorOps[N: Numeric : ClassTag](v: Vector[N]) {
    require(v.length > 0, "Vectors must have 1 or more elements")

    def +(that: Vector[N]): Vector[N]
      = vectorAdd(v, that)

    def -(that: Vector[N]): Vector[N]
      = vectorSub(v, that)

    def *(that: Vector[N]): N
      = dotProduct(v, that)

    def +(s: N): Vector[N]
      = vectorScalarAdd(v, s)

    def -(s: N): Vector[N]
      = vectorScalarSub(v,s)

    def *(s: N): Vector[N]
      = vectorScalarMul(v, s)
  }

  implicit class FractionalVectorOps[F: Fractional : ClassTag](v: Vector[F]) {
    def /(s: F): Vector[F]
      = vectorScalarDiv(v, s)
  }

  implicit class RightScalarVectorOps[N : Numeric : ClassTag](s: N) {

    def +(v: Vector[N]): Vector[N]
    = vectorScalarAdd(v, s)

    def -(v: Vector[N]): Vector[N]
    = vectorScalarSub(v, s)

    def *(v: Vector[N]): Vector[N]
    = vectorScalarMul(v, s)
  }

  implicit class RightFractionalVectorOps[F: Fractional : ClassTag](s: F) {
    def /(v: Vector[F]): Vector[F]
      = vectorScalarDiv(v, s)
  }

  implicit class MatrixOps[N: Numeric : ClassTag](m: Matrix[N]) {
    require(m.length > 0, "Matrices must have 1 or more columns")
    require(m(0).length > 0, "Matrices must have 1 or more rows")

    def +(that: Matrix[N]): Matrix[N]
      = matrixAdd(m, that)

    def -(that: Matrix[N]): Matrix[N]
      = matrixSub(m, that)

    def *(that: Matrix[N]): Matrix[N]
      = crossProduct(m, that)

    def +(s: N): Matrix[N]
      = matrixScalarAdd(m, s)

    def -(s: N): Matrix[N]
      = matrixScalarSub(m, s)

    def *(s: N): Matrix[N]
      = matrixScalarMul(m, s)
  }

  implicit class FractionalMatrixOps[F: Fractional : ClassTag](m: Matrix[F]) {
    def /(s: F): Matrix[F]
      = matrixScalarDiv(m, s)
  }


  implicit class RightScalarMatrixOps[N : Numeric : ClassTag](s: N) {

    def +(m: Matrix[N]): Matrix[N]
      = matrixScalarAdd(m, s)

    def -(m: Matrix[N]): Matrix[N]
      = matrixScalarSub(m, s)

    def *(m: Matrix[N]): Matrix[N]
      = matrixScalarMul(m, s)
  }

  implicit class RightFractionalMatrixOps[F: Fractional : ClassTag](s: F) {
    def /(m: Matrix[F]): Matrix[F]
      = matrixScalarDiv(m, s)
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
    = { require ( a.length == b.length && a(0).length == b(0).length
                , "Cannot take cross product of matrices of unequal size" )
        (for ( row ← a.par )
          yield for ( col ← b.transpose )
            yield row * col) toArray
      }

}
