package me.hawkweisman.util

import java.io.{PrintWriter, StringWriter}

import scala.language.{postfixOps, implicitConversions}

/**
 * Wraps a standard Java throwable and adds a method to get the stack trace
 * as a string rather than printing it to standard out.
 *
 * @author Hawk Weisman
 *
 * Created by hawk on 6/22/15.
 */
class RichException(val e: Throwable) {
  lazy val stackTraceString: String = {
    val sw = new StringWriter
    e printStackTrace new PrintWriter(sw)
    sw toString
  }
}
object RichException {
  implicit def makeRich(e: Throwable): RichException = new RichException(e)
}
