package me.hawkweisman

import java.io.{ File, FileWriter, PrintWriter }

import me.hawkweisman.io.DSV.ToDSV

import scala.language.reflectiveCalls
import scala.util.Try

/** ==IO==
  * Utilities for performing input and output
  *
  * @author Hawk Weisman
  * Created by hawk on 2/10/16.
  */
package object io {

  /** Execute a closure using a closeable resource.
    *
    * Execute a closure using a closeable resource, ensuring that the
    * resource is closed after the closure is completed.
    *
    * This functions analogously to Python's `with` blocks, but it's
    * automagically extended to all types that provide a method of the
    * form `close(): Unit`, and only takes two lines of code to implement.
    * Aren't types great? :)
    *
    * @param resource the closeable resource
    * @param f the closure to execute with the resource
    * @tparam A the resource type. It exposes a method called `close(): Unit`.
    * @tparam B the return type of the closure
    */
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B
    = try f(resource) finally resource.close()

  trait Writeable {
    def write(s: String): Try[Unit]
    def append(s: String): Try[Unit]

    /** Write a line of delimiter-separated values.
      *
      * @param delim the delimiter to separate the values
      * @param line a series of values
      * @return the result of attempting to write the line to the file
      */
    def writeDSVLine(delim: String = ",")(line: String*): Try[Unit]
      = append( (line mkString delim) + "\n" )

    /** Write a line of comma-separated values.
      *
      * This is just a wrapper around [[writeDSVLine()]] with the delimiter
      * set to the comma character.
      *
      * @param line the series of values to write
      * @return the result of attempting to write the line to the file
      */
    @inline def writeCSVLine(line: String*): Try[Unit]
      = writeDSVLine(delim = ",")(line:_*)

    def writeDSV(delim: String = ",")(obj: ToDSV): Try[Unit]
      = append( obj toDSVLine delim )

    def writeCSV(obj: ToDSV): Try[Unit]
      = append( obj toCSVLine )
  }

  implicit class RichFile(val f: File)
  extends Writeable {

    override def append(s: String): Try[Unit]
      = using(new FileWriter(f)){w => Try(w write s) }

    override def write(s: String): Try[Unit]
      = using(new PrintWriter(f)){w => Try(w write s)}
  }

  implicit class RichPath(val path: String)
  extends Writeable {
    private[this] lazy val f: File = new File(path)
    override def append(s: String): Try[Unit]  = f append s
    override def write(s: String): Try[Unit] = f write s
  }

}
