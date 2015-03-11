package me.hawkweisman.common

import scala.annotation.tailrec

/**
 * Miscellaneous mathematical and statistical functionality.
 * @author Hawk Weisman <hi@hawkweisman.me>
 */
package object math {

  /**
   * Tail-recursive K-nearest neighbor search
   * @param k the number of nearest neighbors
   * @param xs a `Set` of `T` from which to find the nearest neighbors
   * @param dist the distance function
   * @tparam T the type of the item being searched
   * @return a set containing the nearest _k_ values
   */
  def kNearest[T](k: Int, xs: Set[T])(dist: (T) => Comparable): Set[T] = {
    require( k >= 0, "Values of k must be greater than zero" )
    @tailrec def _kNearest(k: Int, xs: Set[T], neighbors: Set[T]): Set[T] = {
      val nearest = xs minBy dist
      k match {
        case 0 => neighbors + nearest
        case _ => _kNearest(k - 1, xs - nearest, neighbors + nearest)
      }
    }
    _kNearest(k, xs, Set())
  }

  def kNearest(k: Int, target: Double, xs: Set[Double]): Set[Double] = kNearest[Double](k, xs)((it: Double) => it - target)
  def kNearest(k: Int, target: Int, xs: Set[Int]): Set[Int] = kNearest[Int](k, xs)((it: Int) => it - target)
  def kNearest(k: Int, target: Float, xs: Set[Float]): Set[Float] = kNearest[Float](k, xs)((it: Float) => it - target)
  def kNearest(k: Int, target: Long, xs: Set[Long]): Set[Long] = kNearest[Long](k, xs)((it: Long) => it - target)

}
