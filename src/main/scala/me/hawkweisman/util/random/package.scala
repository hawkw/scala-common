package me.hawkweisman.util

import scala.util.Random

/**
 * ==Random==
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
  def randomString(alphabet: String)(n: Int)(random: Random): String
    = { require(n > 0, "Desired length must be positive")
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
   * [[https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.8
   * Section 3.8]]
   * of the Java SE 8 spec.
   *
   * @param n the length for the [[String]]
   * @param random an instance of [[scala.util.Random]]
   * @return a [[String]] of length _n_
   */
  def randomAlphanumericString(n: Int)(random: Random): String
    = randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)(random)

  /**
   * Generates a random [[String]] that is a valid Java identifier
   * @param n the length of the [[String]]
   * @param random an instance of [[scala.util.Random]]
   * @return a [[String]] of length _n_
   */
  def randomJavaIdent(n: Int)(random: Random): String
    = { val first = randomString(
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$")(1)(random)
        val rest = if (n > 1) {
          randomString(
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$12345667890"
          )(n - 1)(random)
        } else { "" }
        s"$first$rest"
      }

  /**
   * Chooses one of two weighted random options.
   *
   * Both choices are represented as a tuple containing a weight (a double-precision
   * number between 0.0 and 1.0) and a function `() => T`. These functions will not be
   * evaluated unless chosen, allowing computationally expensive choices to be generated only
   * when needed. Also, additional random data generation functions (including other weighted
   * generators) may be passed, allowing the complex chaining of random data generators.
   *
   * @param  a `(Double, () => T)`
   * @param  b `(Double, () => T)`
   * @param  random an instance of [[scala.util.Random]]
   * @return the result of evaluating either `a` or `b`
   */
  def weightedPick2[T]( a: (Double, () ⇒ T)
                      , b: (Double, () ⇒ T))
                      ( implicit random: Random): T
    = (a, b) match {
      case ((aWeight,aFunc), (bWeight, bFunc)) ⇒
        require(aWeight + bWeight == 1.0,
          "The sum of the weights for a and b must equal 1.0")
        require(aWeight > 0,
          "Weight for a must be greater than zero.")
        require(bWeight > 0,
          "Weight for b must be greater than zero.")
        val choices = Seq(a,b) sortWith { case ((x,_), (y,_)) => x > y }
        val (firstWeight, firstResult) = choices(0)
        val (_, secondResult)          = choices(1)
        random.nextDouble() match {
          case i: Double if i <= firstWeight  ⇒ firstResult()
          case _                              ⇒ secondResult()
          // TODO: consider just making this return the chosen function
          // so that it can be evaluated at the call site?
        }
    }
//
//  private[this] def sortByFirst[A : Ordering, B](xs: Seq[(A, B)])
//    = implicitly.Ordering[A]( xs sortWith { case ((x,_), (y,_)) => x > y } )

  /**
   * Chooses one of _n_ weighted random options.
   *
   * The choicees are represented as a sequence of tuples containing a weight
   * (a double-precision number between 0.0 and 1.0) and a function
   * `() => T`. These functions will not be evaluated unless chosen, allowing
   * computationally expensive choices to be generated only when needed.
   *
   * @param  choices `Seq[(Double, () => T)]` a list of (weight, result)
   *                 choices from which to select
   * @param  random an instance of [[scala.util.Random]]
   * @return the result of evaluating the chosen generator
   */
  def weightedPickN[T](choices: Seq[(Double, () ⇒ T)])
                      (implicit random: Random): T
    = { require(choices.length >= 2, "Two or more choices must be provided.")
        choices sortWith { case ((x,_), (y,_)) => x > y } match {
          case Seq(max, min) ⇒ weightedPick2(max,min)(random)
          case Seq(max, rest @ _*) ⇒
            val remainingWeight: Double
              = rest.map { case ((weight, _)) ⇒ weight }
                    .sum
            val (maxWeight, _) = max
            require(remainingWeight + maxWeight == 1.0,
              "The sum of each weight must equal 1.0")
            val choiceRest = rest map {
              case ((weight, f)) ⇒ (weight / 1.0, f)
            }
            weightedPick2(max,
              (remainingWeight, () ⇒ { weightedPickN(choiceRest)(random) })
            )(random)
          case Nil ⇒
            throw new IllegalArgumentException("cannot pick from empty list")
        }
      }

}
