package me.hawkweisman

import java.io.{PrintWriter, StringWriter}

import scala.language.{postfixOps, implicitConversions}
import scala.util.{Try,Success,Failure}

/**
 * ==Utilities==
 * Miscellaneous robust and composeable implementations of various little
 * utility functions that I find myself using over and over again.
 *
 * ==Implicit Conversions==
 * This package contains implicit conversions and enriched implicit classes
 * of standard library classes in the `scala.util` package. These mostly deal
 * with exceptions and error handling. These enriched implicit classes include
 * the following:
 *
 *  1. [[RichException]], which adds a `stackTraceString` value to
 *     [[java.util.Exception]]s, allowing the string  printed by
 *     [[java.util.Exception.printStackTrace Exception.printStackTrace]] to be
 *     accessed programmatically as well as printed to standard error.
 * {{{
 * import me.hawkweisman.util.RichException
 * val e = new Exception("A message")
 * val s: String = e.stackTraceString
 * }}}
 *
 *  2. [[TryWithFold]], which adds the [[TryWithFold.fold fold]] method to
 *     [[scala.util.Try]]. This method will be added to the standard library
 *     in Scala 2.12
 *     (see [[https://issues.scala-lang.org/browse/SI-8336 SI-8336]]).
 * {{{
 * import me.hawkweisman.util.TryWithFold
 * val result: Try[Throwable, Int] = Try { string.toInt }
 * log(result.fold(
 *   ex => "Operation failed with " + ex,
 *   v  => "Operation produced value: " + v
 * ))
 * }}}
 *
 *
 * @example
 *
 * @author Hawk Weisman
 * @since v0.0.3
 */
package object util {
  /**
   * Implicitly adds the `fold` method to [[scala.util.Try]].
   *
   * The `Try.fold()` method will be added to the standard library in
   * Scala 2.12. This implicit simply makes it available in earlier Scala
   * versions, since Scala 2.12 is still in beta. Use of this will be
   * deprecated when Scala 2.12 is officially released.
   *
   * @param  t: A `Try[T]`
   * @return a wrapper adding the `fold` method to `t`
   * @see [[https://issues.scala-lang.org/browse/SI-8336 SI-8336]]
   * @since v0.0.3
   * @author Hawk Weisman
   *
   * }}}
   */
  implicit class TryWithFold[T](val t: Try[T]) {
    /**
     * Applies `fa` if this is a `Failure` or `fb` if this is a `Success`.
     * If `fb` is initially applied and throws an exception,
     * then `fa` is applied with this exception.
     *
     * Note that the signature of `fa` should, really, in my opinion be
     * `NonFatal => U`, rather than `Throwable => U`, and should throw
     * any fatal exceptions encountered. However, since this implicit is
     * providing a feature that will eventually be added in Scala 2.12,
     * I maintained the standard library's definition of this method
     * signature, so as to not break existing code when migrating to
     * Scala 2.12.
     *
     * @example {{{
     * import me.hawkweisman.util.TryWithFold
     * val result: Try[Throwable, Int] = Try { string.toInt }
     * log(result.fold(
     *   ex => "Operation failed with " + ex,
     *   v => "Operation produced value: " + v
     * ))
     * }}}
     *
     * @param fa the function to apply if this is a `Failure`
     * @param fb the function to apply if this is a `Success`
     * @return the results of applying the function
     */
    def fold[U](fa: Throwable => U, fb: T => U): U
      = t match {
        case Failure(why: Throwable)  => fa(why)
        case Success(value)           => fb(value)
      }
  }

  /**
   * Wraps a standard Java throwable and adds a method to get the stack trace
   * as a string rather than printing it to standard out.
   *
   * @since v0.0.3
   * @author Hawk Weisman
   * @example {{{
   * import me.hawkweisman.util.RichException
   * val e = new Exception("A message")
   * val s: String = e.stackTraceString
   * }}}
   * Created by hawk on 6/22/15.
   */
  implicit class RichException(val e: Throwable) {
    lazy val stackTraceString: String = {
      val sw = new StringWriter
      e printStackTrace new PrintWriter(sw)
      sw toString
    }
  }

}
