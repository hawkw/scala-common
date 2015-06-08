package me.hawkweisman.util

/**
 * Random
 * ======
 *
 * Utility methods for generating random data. These can be used in testing,
 * for generating values for cryptographic purposes, or for probabilistic
 * simulators or content generators.
 */
package object random {
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
    val rest = if (n > 1) {
      randomString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$12345667890")(n - 1)(random)
    } else { "" }
    s"$first$rest"
  }

  def weightedPick2[T](a: (Double, () => T), b: (Double, () => T))(random: scala.util.Random): T = {
    require(a._1 + b._1 == 1.0, "The sum of the weights for a and b must equal 1.0")
    require(a._1 > 0, "A must be greater than zero.")
    require(b._1 > 0, "B must be greater than zero.")
    val choices = Seq(a,b) sortWith { case ((x,_), (y,_)) => x > y }
    random.nextDouble match {
      case i if i <= choices(0)._1 => choices(0)._2()
      case _                       => choices(1)._2()
    }
  }

  def weightedPickN[T](choices: List[(Double, () => T)])(random: scala.util.Random): T = choices
  .sortWith { case ((x,_), (y,_)) => x > y } match {
    case max :: min :: Nil => weightedPick2(max,min)(random)
    case max :: rest =>
      val weightRest = rest map (_._1) sum
      val choiceRest = () => { weightedPickN(rest)(random) }
      weightedPick2(max, (weightRest, choiceRest))(random)
  }
}
