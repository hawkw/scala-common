package me.hawkweisman.math

import Matrix.{ map2
              }
import Function.tupled

import scala.reflect.ClassTag

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
class Matrix[N: Numeric](protected val m: Array[Array[N]])
                        (implicit ev1: ClassTag[N]){

  lazy val columns: Int = m length
  lazy val rows: Int = m(0) length

  type Vector = Array[N]

  @inline def isSameSizeAs(that: Matrix[N]): Boolean
    = this.columns == that.columns && this.rows == that.rows

  @inline private[this] def zipMap(that: Matrix[N])(f: (N, N) ⇒ N): Matrix[N]
    = new Matrix[N]( this.m zip that.m map {
        case ((r1: Array[N], r2: Array[N])) ⇒ r1 zip r2 map { Function tupled f }
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

  def times (that: Matrix[N]): Matrix[N]
    = { require(this isSameSizeAs that,
                "Cannot multiply matrices of unequal size")
        val that_prime = that.m.transpose
        new Matrix[N](this.m map {r ⇒ that_prime.map{c ⇒ dot(r, c)}})
      }

  def dot (a: Vector, b: Vector): N
    = a zip b map tupled (implicitly[Numeric[N]].times(_, _)) sum

  def + (that: Matrix[N]): Matrix[N]
    = this plus that

  def - (that: Matrix[N]): Matrix[N]
    = this minus that

  def * (that: N): Matrix[N]
    = this times that

  def * (that: Matrix[N]): Matrix[N]
    = this times that

}

object Matrix {
  @inline protected def map2[A, B](a: Array[Array[A]])(f: A ⇒ B)
                                  (implicit ev1: ClassTag[B]): Array[Array[B]]
    = a map (_ map f)
}
