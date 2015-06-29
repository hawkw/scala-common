package me.hawkweisman.util
package tests

import collection.RepeatableSeq
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
 * Created by hawk on 5/20/15.
 */
class CollectionSpec extends FlatSpec
  with GeneratorDrivenPropertyChecks
  with Matchers {

  "A RepeatableSeq" should "repeat an arbitrary sequence of integers to an arbitrary length" in {
    forAll { (s: Seq[Int]) =>
      whenever (s.nonEmpty && s.length <= 50) {  // 50 seems reasonable?
        val expected = for {
          i <- 0 to 49
        } yield s (i % s.length)

        s.repeat.take(50) shouldEqual expected.toStream
      }
    }
  }

}
