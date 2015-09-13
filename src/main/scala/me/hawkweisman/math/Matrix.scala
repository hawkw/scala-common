package me.hawkweisman.math

import Matrix.{ map2
              , TwoD
              }
/**
 * A generic arbitrary-sized matrix implementation, backed by
 * a multidimensional array.
 *
 * As this is implemented in Scala, it cannot be as fast as native code
 * implementations such as LAPPACK. However, it should be fairly high
 * performance, as care was taken to ensure that the compiler can paralellize
 * a majority of matrix operations.
 *
 * Created by hawk on 9/13/15.
 */
class Matrix[N: Numeric](private val m: TwoD[N]) {

  lazy val columns: Int = m length
  lazy val rows: Int = m(0) length

  @inline def isSameSizeAs(that: Matrix[N]): Boolean
    = this.columns == that.columns && this.rows == that.rows

  @inline private[this] def zipMap(that: Matrix[N])(f: (N, N) ⇒ N): Matrix[N]
    = new Matrix[N]( this.m zip that.m map {
        case ((r1, r2)) ⇒ r1 zip r2 map { Function tupled f }
      })

  def plus (that: Matrix[N]): Matrix[N]
    = { require(this isSameSizeAs that,
                "Cannot add matrices of unequal size")
        zipMap(that)( (a: N, b: N) ⇒ implicitly[Numeric[N]].plus(a,b) )
      }

  def minus (that: Matrix[N]): Matrix[N]
    = { require(this isSameSizeAs that,
              "Cannot subtract matrices of unequal size")
        zipMap(that)( (a: N, b: N) ⇒ implicitly[Numeric[N]].minus(a,b) )
      }

  def times (scalar: N): Matrix[N]
    = new Matrix[N]( map2(m)(implicitly[Numeric[N]].times(_, scalar)) )

  def cross_product (that: N): Matrix[N]
    = ???

  def dot_product (that: N): Matrix[N]
    = ???

  def + (that: Matrix[N]): Matrix[N]
    = this plus that

  def - (that: Matrix[N]): Matrix[N]
    = this minus that

  def * (that: N): Matrix[N]
    = this times that

}

object Matrix {
  type TwoD[T] = Array[Array[T]]

  @inline protected def map2[A, B](a: TwoD[A])(f: A ⇒ B): TwoD[B]
    = a map (_ map f)
}
