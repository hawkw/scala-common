package me.hawkweisman
package io

import util.EscapableString

import scala.language.postfixOps

/**
  * Created by hawk on 2/10/16.
  */
object DSV {

  implicit class ToDSV(val p: Product)
  extends AnyVal {

    def toDSVLine(delim: String = ","): String
      = p.productIterator map {
          case Some(thing) => thing
          case None => ""
          case string: String => string escaped
          case value => value
        } mkString delim + "\n"

    def toCSVLine: String = toDSVLine(delim = ",")
  }

}
