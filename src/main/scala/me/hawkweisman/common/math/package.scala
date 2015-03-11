package me.hawkweisman.common

import scala.annotation.tailrec
import scala.math.abs

/**
 * Miscellaneous mathematical and statistical functionality.
 * @author Hawk Weisman <hi@hawkweisman.me>
 */
package object math {

  /**
   * Tail-recursive _k_-nearest-neighbor search.
   *
   * This implementation uses a distance function with the signature `(T) => Comparable`
   * to compute the distance from the target to the element in question. There are helper
   * functions which will provide this distance function automatically if you want to
   * do _k_-nearest-neighbor search over a set of `Int`s, `Float`s, `Long`s or `Double`s,
   * which accept a target as a parameter and construct a distance function using the
   * absolute value of the difference between the element and the target. If you want to
   * perform _k_-nearest searches across sets of other types, you'll have to roll your
   * own distance function. While this seems like a pain, it also means that this search
   * algorithm should be applicable to pretty much anything - you can do nearest-neighbor
   * searches across `List`s or `String`s or bitmaps or literally any other thing you can
   * write a distance function for.
   *
   * This tail-recursive implementation should be approximately the most performant
   * possible algorithm for performing _k_-nearest search without preprocessing the inputs.
   *
   * @param k the number of nearest neighbors
   * @param xs a `Set` of `T` from which to find the nearest neighbors
   * @param dist the distance function
   * @tparam A the type of the item being searched
   * @tparam B the type of the distance result
   * @return a set containing the nearest _k_ values
   */
  def kNearest[A,B <% Ordered[B]](k: Int, xs: Set[A])(dist: (A) => B): Set[A] = {
    require(k >= 0, "Values of k must be greater than zero")
    @tailrec def _kNearest(k: Int, xs: Set[A], neighbors: Set[A]): Set[A] = {
      val nearest = xs minBy dist
      k match {
        case 0 => neighbors + nearest
        case _ => _kNearest(k - 1, xs - nearest, neighbors + nearest)
      }
    }
    _kNearest(k, xs, Set())
  }

  def kNearest(k: Int, target: Double, xs: Set[Double]): Set[Double] =
    kNearest[Double,Double](k, xs)((it: Double) => abs(it - target))

  def kNearest(k: Int, target: Int, xs: Set[Int]): Set[Int] =
    kNearest[Int,Int](k, xs)((it: Int) => abs(it - target))

  def kNearest(k: Int, target: Float, xs: Set[Float]): Set[Float] =
    kNearest[Float,Float](k, xs)((it: Float) => abs(it - target))

  def kNearest(k: Int, target: Long, xs: Set[Long]): Set[Long] =
    kNearest[Long,Long](k, xs)((it: Long) => abs(it - target))

}
