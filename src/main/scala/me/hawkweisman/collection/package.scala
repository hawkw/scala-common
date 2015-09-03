package me.hawkweisman
/**
 * ==Collection==
 * Contains various utilities for working on collections and implementations
 * of collection data structures.
 *
 * ==Implicit Conversions==
 * This package contains implicit conversions and enriched implicit classes
 * for standard library classesin the `scala.collection` package.
 *
 *  1. [[collection.RepeatableSeq RepeatableSeq]] enriches instances of
 *     [[scala.collection.Seq]] with a
 *     [[collection.RepeatableSeq.repeat() repeat()]] method that
 *     constructs an infinite [[scala.collection.immutable.Stream Stream]]
 *     repeating that sequence of items.
 *  {{{
 *  import me.hawkweisman.util.collection.RepeatableSeq
 *  val s = Seq[Int](1,2,3,4)
 *  val r: Stream[Int] = seq.repeat()
 *  }}}
 *  2. [[collection.MapAccumable MapAccumable]] enriches instances of
 *     [[scala.collection.Seq]] with
 *     [[collection.MapAccumable.mapAccumLeft() mapAccumLeft()]] and
 *     [[collection.MapAccumable.mapAccumRight() mapAccumRight()]] methods
 *     like Haskell's `mapAccumL` and `mapAccumR`.
 *
 * @author Hawk Weisman
 */
package object collection {

  /**
   * Enhances the standard library [[scala.collection.Seq Seq]] type with the
   * ability to create an infinite [[scala.collection.immutable.Stream Stream]]
   * of repetitions of its values.
   * @param self a `Seq[A]`
   * @tparam A the type of the items in this [[scala.collection.Seq Seq]]
   */
  implicit class RepeatableSeq[A](self: Seq[A]) {
    /**
     * Constructs a new infinite [[scala.collection.immutable.Stream Stream]]
     * that repeats this sequence of values.
     * @return an infinite [[scala.collection.immutable.Stream Stream]] of
     *         repetitions of this sequence
     * @example{{{
     *  import me.hawkweisman.util.collection.RepeatableSeq
     *  val s = Seq[Int](1,2,3,4)
     *  val r: Stream[Int] = seq.repeat()
     *  }}}
     */
    def repeat: Stream[A]
      = Stream.continually(self)
              .flatten
  }


  /**
   * Enhances the standard library [[scala.collection.Seq Seq]] type with the
   * `mapAccum` functions like in Haskell.
   * @param self a `Seq[A]`
   * @tparam A the type of the items in this [[scala.collection.Seq Seq]]
   */
  implicit class MapAccumable[A](self: Seq[A]) {

    /**
     * This behaves like a combination of [[scala.collection.Seq.map() map()]]
     * and [[scala.collection.Seq.foldLeft() foldLeft()]]. It applies a function
     * to each element of this sequence, passing an accumulating parameter from
     * left to right, and returning a final value of this accumulator together
     * with the new sequence.
     *
     * This is ported from the Haskell `mapAccumL` function.
     *
     * @param z the initial value of the accumulator
     * @param op the operation to apply
     * @tparam B the type of the accumulator
     * @tparam C the type of the elements in the mapped sequence
     * @return a tuple containing the final value of the accumulator
     *         and the mapped sequence
     */
    def mapAccumLeft[B, C](z: B)(op: (B, A) ⇒ (B, C)): (B, Seq[C])
      = self match {
          case Seq() ⇒ (z, Seq[C]())
          case Seq(x, xs@_*) ⇒
            val (z_prime, y) = op(z, x)
            val (z_prime_prime, ys) = xs.mapAccumLeft(z_prime)(op)
            (z_prime_prime, y +: ys)
        }

    /**
     * This behaves like a combination of [[scala.collection.Seq.map() map()]]
     * and [[scala.collection.Seq.foldRight() foldRight()]]. It applies a
     * function to each element of this sequence, passing an accumulating
     * parameter from right to left, and returning a final value of this
     * accumulator together with the new sequence.
     *
     * This is ported from the Haskell `mapAccumR` function.
     *
     * @param z the initial value of the accumulator
     * @param op the operation to apply
     * @tparam B the type of the accumulator
     * @tparam C the type of the elements in the mapped sequence
     * @return a tuple containing the final value of the accumulator
     *         and the mapped sequence
     */
    def mapAccumRight[B, C](z: B)(op: (A, B) ⇒ (B, C)): (B, Seq[C])
      = self.reverse
            .mapAccumLeft(z)((right, left) => op(left, right))

  }

}
