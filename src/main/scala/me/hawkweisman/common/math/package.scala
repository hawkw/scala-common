package me.hawkweisman.common

import scala.annotation.tailrec

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
   * @tparam T the type of the item being searched
   * @return a set containing the nearest _k_ values
   */
  def kNearest[T](k: Int, xs: Set[T])(dist: (T) => Comparable[T]): Set[T] = {
    require(k >= 0, "Values of k must be greater than zero")
    @tailrec def _kNearest(k: Int, xs: Set[T], neighbors: Set[T]): Set[T] = {
      val nearest = xs minBy dist
      k match {
        case 0 => neighbors + nearest
        case _ => _kNearest(k - 1, xs - nearest, neighbors + nearest)
      }
    }
    _kNearest(k, xs, Set())
  }

  def kNearest(k: Int, target: Double, xs: Set[Double]): Set[Double] =
    kNearest[Double](k, xs)((it: Double) => scala.math.abs(it - target))

  def kNearest(k: Int, target: Int, xs: Set[Int]): Set[Int] =
    kNearest[Int](k, xs)((it: Int) => scala.math.abs(it - target))

  def kNearest(k: Int, target: Float, xs: Set[Float]): Set[Float] =
    kNearest[Float](k, xs)((it: Float) => scala.math.abs(it - target))

  def kNearest(k: Int, target: Long, xs: Set[Long]): Set[Long] =
    kNearest[Long](k, xs)((it: Long) => scala.math.abs(it - target))

}
