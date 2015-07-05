package me.hawkweisman.util
/**
 * Collection
 * ==========
 *
 * Contains various personal utilities for working on collections.
 *
 * @author Hawk Weisman
 */
package object collection {

  /**
   * Enhances the standard library [[Seq]] type with the ability to stream repetitions.
   * @param self a `Seq[A]`
   * @tparam A the type of this [[Seq]]
   */
  implicit class RepeatableSeq[A](self: Seq[A]) {
    /**
     * Constructs a new infinite [[Stream]] that repeats this sequence of values.
     * @return an infinite stream of repetitions of this sequence
     */
    def repeat: Stream[A] = Stream.continually(self).flatten
  }

}
