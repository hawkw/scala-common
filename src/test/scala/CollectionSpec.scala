import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import me.hawkweisman.util.collection.RepeatableSeq

/**
 * Created by hawk on 5/20/15.
 */
class CollectionSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  "A RepeatableSeq" should "repeat an arbitrary sequence of integers to an arbitrary length" in {
    forAll { (s: Seq[Int], length: Int) =>
      val expected = for {
        i <- 0 to length
        e <- s (i % s.length)
      } yield e

      s take length shouldEqual expected
    }
  }

}
