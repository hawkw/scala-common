package me.hawkweisman

/**
 * Utilities
 * =========
 *
 * Miscellaneous robust and composeable implementations of various little utility
 * functions that I find myself using over and over again.
 */
package object util {

  /**
   * Generates a random [[String]] of length _n_ from the given alphabet
   * @param alphabet A [[String]] containing all the symbols in the alphabet
   * @param n the length of the [[String]]
   * @param random an instance of [[scala.util.Random]]
   * @return a [[String]] of length _n_
   */
  def randomString(alphabet: String)(n: Int)(random: scala.util.Random): String = {
    require(n > 0, "Desired length must be positive")
    require(alphabet != "", "Alphabet must contain characters")
    Stream.continually(random.nextInt(alphabet.length))
      .map(alphabet)
      .take(n)
      .mkString
  }

  /**
   * Generates a random [[String]] of length _n_ consisting of lowercase
   * letters and numbers.
   *
   * Identifiers are generated based on
   * [[https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.8 Section 3.8]]
   * of the Java SE 8 spec.
   *
   * @param n the length for the [[String]]
   * @param random an instance of [[scala.util.Random]]
   * @return a [[String]] of length _n_
   */
  def randomAlphanumericString(n: Int)(random: scala.util.Random): String =
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)(random)

  /**
   * Generates a random [[String]] that is a valid Java identifier
   * @param n the length of the [[String]]
   * @param random an instance of [[scala.util.Random]]
   * @return a [[String]] of length _n_
   */
  def randomJavaIdent(n: Int)(random: scala.util.Random): String = {
    val first = randomString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$")(1)(random)
    val rest = randomString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$12345667890")(n-1)(random)
    s"$first$rest"
  }
}
