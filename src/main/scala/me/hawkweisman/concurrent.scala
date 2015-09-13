package me.hawkweisman

import scala.concurrent.{ Promise
                        , Future
                        }
import scala.util.{ Try
                  , Success
                  , Failure
                  }

/**
 * ==Concurrent==
 * Utilities for concurrent programming.
 *
 * ==Implicit Conversions==
 * This package contains implicit conversions for standard library classes
 * in the `scala.concurrent` package.
 *
 *  1. [[TryToFuture]] implicitly converts an instance of [[scala.util.Try]]
 *     to an instance of [[scala.concurrent.Future]].
 *  {{{
 *  import scala.util.Try
 *  import scala.concurrent.Future
 *  import me.hawkweisman.util.concurrent.tryToFuture
 *
 *  def tryToGetAString(): Try[String]
 *    = Success("hi!")
 *
 *  val stringFuture: Future[String] = tryToGetAString()
 *  }}}
 *
 */
object concurrent {

  /**
   * Implicitly convert a [[scala.util.Try Try]] to a
   * [[scala.concurrent.Future Future]].
   *
   * This is primarily for use when you are `flatMap`ping over multiple
   * `Future`s and you want to include some non-async operations.
   *
   * For best performance, the operation that returns a `Try` rather than
   * a `Future` should either be the first item `flatMap`ped over, or should
   * have already been performed and stored in a local value. If it is the
   * result of a function call and included after multiple async calls,
   * I believe all the async operations will block on that call.
   *
   * @param  t A [[scala.util.Try Try]]
   * @return that [[scala.util.Try Try]] converted to a
   *         [[scala.concurrent.Future]]
   * @example{{{
   *  import scala.util.Try
   *  import scala.concurrent.Future
   *  import me.hawkweisman.util.concurrent.tryToFuture
   *
   *  def tryToGetAString(): Try[String]
   *    = Success("hi!")
   *
   *  val stringFuture: Future[String] = tryToGetAString()
   *  }}}
   */
  implicit def tryToFuture[T](t: Try[T]): Future[T]
    = Promise().complete(t)
               .future

}
