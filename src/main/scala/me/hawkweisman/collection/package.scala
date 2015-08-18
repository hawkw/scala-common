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
 *  1. [[RepeatableSeq]] enriches instances of [[scala.collection.Seq]]
 *     with a [[RepeatableSeq.repeat]] method that constructs an infinite
 *     [[scala.collection.immutable.Stream Stream]] repeating that sequence
 *     of items.
 *  {{{
 *  import me.hawkweisman.util.collection.RepeatableSeq
 *  val s = Seq[Int](1,2,3,4)
 *  val r: Stream[Int] = seq.repeat()
 *  }}}
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

}
