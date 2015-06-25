package me.hawkweisman.util

import scala.concurrent.{Promise, Future}
import scala.util.{Try,Success,Failure}

package object concurrent {

  implicit def tryToFuture[T](t: Try[T]): Future[T] = Promise().complete(t).future

}
