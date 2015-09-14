package me.hawkweisman.math.linear

import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.Function._

/**
 * Created by hawk on 9/13/15.
 */
trait Linear {

  type Vector[N] = Array[N]
  type Matrix[N] = Vector[Vector[N]]

  def vectorAdd[N: Numeric](a: Vector[N], b: Vector[N])
                           (implicit ev1: ClassTag[N]): Vector[N]

  def vectorSub[N: Numeric](a: Vector[N], b: Vector[N])
                           (implicit ev1: ClassTag[N]): Vector[N]

  def dotProduct[N: Numeric](a: Vector[N], b: Vector[N])
                            (implicit ev1: ClassTag[N]): N

  def matrixAdd[N: Numeric](a: Matrix[N], b: Matrix[N])
                           (implicit ev1: ClassTag[N]): Matrix[N]

  def matrixSub[N: Numeric](a: Matrix[N], b: Matrix[N])
                           (implicit ev1: ClassTag[N]): Matrix[N]

  def crossProduct[N: Numeric](a: Matrix[N], b: Matrix[N])
                              (implicit ev1: ClassTag[N]): Matrix[N]


  implicit class VectorOps[N: Numeric](v: Vector[N])(implicit ev1: ClassTag[N])
  {

    def +(that: Vector[N]): Vector[N]
      = vectorAdd(v, that)

    def -(that: Vector[N]): Vector[N]
      = vectorSub(v, that)

    def *(that: Vector[N]): N
      = dotProduct(v, that)
  }

  implicit class MatrixOps[N: Numeric](m: Matrix[N])(implicit ev1: ClassTag[N])
  {

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
  private[this] def zipMap[N: Numeric](a: Matrix[N], b: Matrix[N])
                                      (f: (N, N) ⇒ N)
                                      (implicit ev1: ClassTag[N]): Matrix[N]
    = a zip b map { case ((r1: Array[N], r2: Array[N])) ⇒
        r1 zip r2 map tupled (f)
      }

  override def vectorAdd[N: Numeric](a: Vector[N], b: Vector[N])
                                    (implicit ev1: ClassTag[N]): Vector[N]
    = a zip b map tupled (implicitly[Numeric[N]].plus(_, _))

  override def vectorSub[N: Numeric](a: Vector[N], b: Vector[N])
                                    (implicit ev1: ClassTag[N]): Vector[N]
    = a zip b map tupled (implicitly[Numeric[N]].minus(_, _))

  override def dotProduct[N: Numeric](a: Vector[N], b: Vector[N])
                                     (implicit ev1: ClassTag[N]): N
    = a zip b map tupled (implicitly[Numeric[N]].times(_, _)) sum

  override def matrixAdd[N: Numeric](a: Matrix[N], b: Matrix[N])
                                    (implicit ev1: ClassTag[N]): Matrix[N]
    = zipMap(a, b)((a: N, b: N) ⇒ implicitly[Numeric[N]].plus(a, b))

  override def matrixSub[N: Numeric](a: Matrix[N], b: Matrix[N])
                                    (implicit ev1: ClassTag[N]): Matrix[N]
    = zipMap(a, b)((a: N, b: N) ⇒ implicitly[Numeric[N]].minus(a, b))

  override def crossProduct[N: Numeric](a: Matrix[N], b: Matrix[N])
                                       (implicit ev1: ClassTag[N]): Matrix[N]
    = for ( row ← a )
        yield for ( col ← b transpose )
          yield row * col

}

trait ParallelAlgebra
extends Linear {

  override def vectorAdd[N: Numeric](a: Vector[N], b: Vector[N])
                                    (implicit ev1: ClassTag[N]): Vector[N]
    = { require( a.length == b.length
               , "Cannot add vectors of unequal length" )
        (for { (n, m) ← a.par zip b.par }
          yield implicitly[Numeric[N]].plus(n, m)) toArray
      }

  override def vectorSub[N: Numeric](a: Vector[N], b: Vector[N])
                                    (implicit ev1: ClassTag[N]): Vector[N]
    = { require( a.length == b.length
               , "Cannot subtract vectors of unequal length" )
        (for { (n, m) ← a.par zip b.par }
          yield implicitly[Numeric[N]].minus(n, m)) toArray
      }

  override def dotProduct[N: Numeric](a: Vector[N], b: Vector[N])
                                     (implicit ev1: ClassTag[N]): N
  = { require( a.length == b.length
             , "Cannot take dot product of vectors of unequal length" )
      (for { (n, m) ← a.par zip b.par }
        yield implicitly[Numeric[N]].minus(n, m)) sum
    }

  override def matrixAdd[N: Numeric](a: Matrix[N], b: Matrix[N])
                                    (implicit ev1: ClassTag[N]): Matrix[N]
    = { require ( a.length == b.length && a(0).length == b(0).length
                , "Cannot add matrices of unequal size" )
        (for {(r1, r2) ← a.par zip b.par}
          yield (for {(x, y) ← r1.par zip r2.par}
            yield implicitly[Numeric[N]].plus(x, y))
            .toArray)
          .toArray
      }

  override def matrixSub[N: Numeric](a: Matrix[N], b: Matrix[N])
                                    (implicit ev1: ClassTag[N]): Matrix[N]
    = { require ( a.length == b.length && a(0).length == b(0).length
                , "Cannot subtract matrices of unequal size" )
        (for {(r1, r2) ← a.par zip b.par}
          yield (for {(x, y) ← r1.par zip r2.par}
            yield implicitly[Numeric[N]].minus(x, y))
            .toArray)
          .toArray
      }

  override def crossProduct[N: Numeric](a: Matrix[N], b: Matrix[N])
                                       (implicit ev1: ClassTag[N]): Matrix[N]
    = (for ( row ← a.par )
        yield for ( col ← b.transpose )
          yield row * col) toArray
}
