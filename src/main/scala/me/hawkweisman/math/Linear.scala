package me.hawkweisman.math

import scala.Function._
import scala.language.postfixOps
import scala.reflect.ClassTag
import Numeric.Implicits._
import Fractional.Implicits._
import scala.math.{ cos, sin }


/**
 * ==Linear==
 *
 * A mixin trait providing a linear algebra DSL.
 *
 * This trait adds vector operations to all arrays whose elements are
 * [[Numeric]], and matrix operations to all two-dimensional arrays whose
 * elements are [[Numeric]].
 *
 * Scalar/vector and scalar/matrix operations are available as well, using a
 * syntax similar to Haskell's
 * [[https://hackage.haskell.org/package/linear-1.20.1/docs/Linear-Vector.html
 * Linear.Vector]] package. When using linear/scalar operators, place the caret
 * (`^`) on the side of the operator facing towards the linear operand, and the
 * side without the caret facing towards the scalar operand.
 *
 * This trait only provides an interface to the linear algebra DSL. In order
 * to actually perform linear operations, you must also mix in one of the
 * implementation traits: either [[SequentialAlgebra]], a naive implementation
 * which uses sequential array traversals, or [[ParallelAlgebra]], which uses
 * Scala's [[scala.collection.parallel parallel collections]] for faster
 * performance.
 *
 * @author Hawk Weisman
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

  def vectorMatrixMul[N: Numeric: ClassTag]
                     (v: Vector[N], m: Matrix[N]): Matrix[N]

  def dotProduct[N: Numeric: ClassTag](a: Vector[N], b: Vector[N]): N

  def matrixAdd[N: Numeric: ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]
  def matrixSub[N: Numeric: ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]

  def matrixScalarAdd[N: Numeric: ClassTag](m: Matrix[N], s: N): Matrix[N]
  def matrixScalarSub[N: Numeric: ClassTag](m: Matrix[N], s: N): Matrix[N]
  def matrixScalarDiv[N: Fractional: ClassTag](m: Matrix[N], s: N): Matrix[N]
  def matrixScalarMul[N: Numeric: ClassTag](m: Matrix[N], s: N): Matrix[N]

  def crossProduct[N: Numeric: ClassTag](a: Matrix[N], b: Matrix[N]): Matrix[N]

  /**
   * Implicit class wrapping an array of [[Numeric]] elements
   * to provide vector algebra operators.
   *
   * [[VectorOps]] provides all vector-vector operations, and left-scalar
   * vector-scalar operations.
   *
   * Ideally, the vector-scalar operations could be performed using the actual
   * `+`, `-`, and `*` operators, but unfortunately, due to the Java Virtual
   * Machine's type erasure, these definitions conflict. Instead, vector-scalar
   * operations used "winged" operators a la Haskell's `Linear.Vector`. The
   * caret character (`^`) goes on the vector side.
   *
   * @param v the wrapped vector
   * @param ev1 evidence that the elements of the array are [[Numeric]]
   * @param ev2 evidence that the elements of the array have [[ClassTag]]s
   * @tparam N the type of elements in the array.
   */
  implicit class VectorOps[N: Numeric : ClassTag](v: Vector[N]) {
    require(v.length > 0, "Vectors must have 1 or more elements")

    def +(that: Vector[N]): Vector[N]
      = vectorAdd(v, that)

    def -(that: Vector[N]): Vector[N]
      = vectorSub(v, that)

    def *(that: Vector[N]): N
      = dotProduct(v, that)

    def *(that: Matrix[N]): Matrix[N]
      = vectorMatrixMul(v, that)

    def ^+(s: N): Vector[N]
      = vectorScalarAdd(v, s)

    def ^-(s: N): Vector[N]
      = vectorScalarSub(v,s)

    def ^*(s: N): Vector[N]
      = vectorScalarMul(v, s)

    override lazy val toString: String
      = "[" + (v mkString ", ") + "]"

    def magnitude: Double
      = Math.sqrt((v * v).asInstanceOf[Double])

  }
  /**
   * Provides additional operators for vectors
   * whose elements are [[Fractional]].
   *
   * Since Scala's [[Numeric]] typeclass doesn't provide a division operator,
   * vector-scalar division is only available on [[Fractional]] vectors and
   * [[Fractional]] scalars.
   *
   * @param v the wrapped vector
   * @param ev1 evidence that the elements of the array are [[Fractional]]
   * @param ev2 evidence that the elements of the array have [[ClassTag]]s
   * @tparam F the type of elements in the array.
   */
  implicit class FractionalVectorOps[F: Fractional : ClassTag](v: Vector[F]) {
    require(v.length > 0, "Vectors must have 1 or more elements")

    def /^(s: F): Vector[F]
      = vectorScalarDiv(v, s)

    def angleTo(vPrime: Vector[F]): Double
      = Math.cos( (v * vPrime).asInstanceOf[Double] /
                  (v.magnitude * vPrime.magnitude) )
  }

  /**
   * Provides vector-scalar operators for a right-hand scalar quantity.
   *
   * Ideally, the vector-scalar operations could be performed using the actual
   * `+`, `-`, and `*` operators, but unfortunately, due to the Java Virtual
   * Machine's type erasure, these definitions conflict. Instead, vector-scalar
   * operations used "winged" operators a la Haskell's `Linear.Vector`. The
   * caret character (`^`) goes on the vector side.
   *
   * @param s the wrapped scalar quantity
   * @param ev1 evidence that the scalar variable is [[Numeric]]
   * @param ev2 evidence that the scalar variable has a [[ClassTag]]
   * @tparam N the type of the scalar quantity
   */
  implicit class RightScalarVectorOps[N : Numeric : ClassTag](s: N) {

    def +^(v: Vector[N]): Vector[N]
    = vectorScalarAdd(v, s)

    def -^(v: Vector[N]): Vector[N]
    = vectorScalarSub(v, s)

    def *^(v: Vector[N]): Vector[N]
    = vectorScalarMul(v, s)
  }

  /**
   * Provides additional vector operators for [[Fractional]]
   * scalar quantities on the right-hand side.
   *
   * Since Scala's [[Numeric]] typeclass doesn't provide a division operator,
   * vector-scalar division is only available on [[Fractional]] vectors and
   * [[Fractional]] scalars.
   *
   * @param s the wrapped scalar quantity
   * @param ev1 evidence that the scalar quantity is [[Fractional]]
   * @param ev2 evidence that the scalar quantity has a [[ClassTag]]s
   * @tparam F the type of the scalar quantity
   */
  implicit class RightFractionalVectorOps[F: Fractional : ClassTag](s: F) {
    def /^(v: Vector[F]): Vector[F]
      = vectorScalarDiv(v, s)
  }

  /**
   * Implicit class wrapping a two-dimensional array of [[Numeric]] elements
   * to provide matrix algebra operations.
   *
   * [[MatrixOps]] provides all matrix-matrix operations, and left-scalar
   * matrix-scalar operations.
   *
   * Ideally, the matrix-scalar operations could be performed using the actual
   * `+`, `-`, and `*` operators, but unfortunately, due to the Java Virtual
   * Machine's type erasure, these definitions conflict. Instead, matrix-matrix
   * operations used "winged" operators a la Haskell's `Linear.Vector`. The
   * caret character (`^`) goes on the matrix side.
   *
   * @param m the wrapped 2D array
   * @param ev1 evidence that the elements of the array are [[Numeric]]
   * @param ev2 evidence that the elements of the array have [[ClassTag]]s
   * @tparam N the type of elements in the array.
   */
  implicit class MatrixOps[N: Numeric : ClassTag](m: Matrix[N]) {
    require(m.length > 0, "Matrices must have 1 or more columns")
    require(m(0).length > 0, "Matrices must have 1 or more rows")

    def +(that: Matrix[N]): Matrix[N]
      = matrixAdd(m, that)

    def -(that: Matrix[N]): Matrix[N]
      = matrixSub(m, that)

    def *(that: Matrix[N]): Matrix[N]
      = crossProduct(m, that)

    def *(that: Vector[N]): Matrix[N]
     = vectorMatrixMul(that, m)

    def ^+(s: N): Matrix[N]
      = matrixScalarAdd(m, s)

    def ^-(s: N): Matrix[N]
      = matrixScalarSub(m, s)

    def ^*(s: N): Matrix[N]
      = matrixScalarMul(m, s)
  }

  /**
   * Provides additional operators for matrices
   * whose elements are [[Fractional]].
   *
   * Since Scala's [[Numeric]] typeclass doesn't provide a division operator,
   * matrix-scalar division is only available on [[Fractional]] matrices and
   * [[Fractional]] scalars.
   *
   * @param m the wrapped 2D array representing the matrix
   * @param ev1 evidence that the elements of the array are [[Fractional]]
   * @param ev2 evidence that the elements of the array have [[ClassTag]]s
   * @tparam F the type of elements in the array.
   */
  implicit class FractionalMatrixOps[F: Fractional : ClassTag](m: Matrix[F]) {
    require(m.length > 0, "Matrices must have 1 or more columns")
    require(m(0).length > 0, "Matrices must have 1 or more rows")

    def ^/(s: F): Matrix[F]
      = matrixScalarDiv(m, s)
  }
  /**
   * Provides matrix-scalar operations for a right-hand scalar quantity.
   *
   * Ideally, the matrix-scalar operations could be performed using the actual
   * `+`, `-`, and `*` operators, but unfortunately, due to the Java Virtual
   * Machine's type erasure, these definitions conflict. Instead, matrix-scalar
   * operations used "winged" operators a la Haskell's `Linear.Vector`. The
   * caret character (`^`) goes on the matrix side.
   *
   * @param s the wrapped scalar quantity
   * @param ev1 evidence that the scalar variable is [[Numeric]]
   * @param ev2 evidence that the scalar variable has a [[ClassTag]]
   * @tparam N the type of the scalar quantity
   */
  implicit class RightScalarMatrixOps[N: Numeric : ClassTag](s: N) {

    def +^(m: Matrix[N]): Matrix[N]
      = matrixScalarAdd(m, s)

    def -^(m: Matrix[N]): Matrix[N]
      = matrixScalarSub(m, s)

    def *^(m: Matrix[N]): Matrix[N]
      = matrixScalarMul(m, s)
  }

  /**
   * Provides additional matrix operators for [[Fractional]]
   * scalar quantities on the right-hand side.
   *
   * Since Scala's [[Numeric]] typeclass doesn't provide a division operator,
   * matrix-scalar division is only available on [[Fractional]] matrices and
   * [[Fractional]] scalars.
   *
   * @param s the wrapped scalar quantity
   * @param ev1 evidence that the scalar quantity is [[Fractional]]
   * @param ev2 evidence that the scalar quantity has a [[ClassTag]]s
   * @tparam F the type of the scalar quantity
   */
  implicit class RightFractionalMatrixOps[F: Fractional : ClassTag](s: F) {
    def /^(m: Matrix[F]): Matrix[F]
      = matrixScalarDiv(m, s)
  }
}

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

  override def vectorMatrixMul[N: Numeric: ClassTag]
                              (v: Vector[N], m: Matrix[N]): Matrix[N]
    = { // currently, this is special cased because
        // unevenly sized vectors isn't done yet
        require(v.length == m(0).length)
        m zip v map {  case ((row, s)) ⇒ row ^* s }

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
        for (row ← a) yield for (col ← b transpose)
          yield row * col
      }

}


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
        m.par zip v.par map { case ((row, s)) ⇒ row ^* s } toArray
    }

}

trait TwoDTransforms
extends Linear {

  def rotationMatrix(theta: Double): Matrix[Double]
    = Array( Array(cos(theta), sin(theta))
           , Array(-sin(theta), cos(theta)))

  def scalingMatrix(sx: Double, sy: Double): Matrix[Double]
    = Array( Array(sx, 0 )
           , Array(0,  sy) )

  def translationMatrix(tx: Double, ty: Double): Vector[Double]
    = Array(tx, ty)

  implicit class Transformable(val v: Vector[Double]) {

    def rotatedBy(degrees: Double): Matrix[Double]
      = rotationMatrix(degrees) * v

    def scaledBy( sx: Double = 0
                , sy: Double = 0): Matrix[Double]
      = scalingMatrix(sx, sy) * v

    def translatedTo( tx: Double = 0
                    , ty: Double = 0): Vector[Double]
      = translationMatrix(tx, ty) + v

  }

}

trait SurfaceNormals
extends Linear {
  def surfaceNormal[N: Numeric: ClassTag]( p1: Vector[N]
                                         , p2: Vector[N]
                                         , p3: Vector[N]): Vector[N]
    = { val u = p2 - p1
        val v = p3 - p1
        Array[N]( (u(1) * v(2)) - (u(2) * v(1))
                , (u(2) * v(0)) - (u(0) * v(2))
                , (u(0) * v(1)) - (u(1) * v(0)) )
      }
}
