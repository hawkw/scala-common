import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import me.hawkweisman.util

/**
 * Created by hawk on 5/9/15.
 */
class UtilSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "The randomAlphanumericString() method" should "generate strings of the correct length" in {
    forAll {
      (length: Int) => whenever (length > 0) {
        util.randomAlphanumericString(length)(new scala.util.Random).length == length
      }
    }
  }
  it should "generate strings in the alphabet [abcdefghijklmnopqrstuvwxyz0123456789]*" in {
    forAll {
      (length: Int) => whenever (length > 0) {
        util.randomAlphanumericString(length)(new scala.util.Random) should fullyMatch regex """[abcdefghijklmnopqrstuvwxyz0123456789]*"""
      }
    }
  }
  "The randomString() method" should "generate strings of the correct length" in {
    forAll {
      (alphabet: String, length: Int) => whenever (alphabet != "" && length > 0) {
        util.randomString(alphabet)(length)(new scala.util.Random).length == length
      }
    }
  }
  it should "generate strings in the specified alphabet" in {
    forAll {
      (alphabet: String, length: Int) => whenever (alphabet != "" && length > 0) {
        util.randomString(alphabet)(length)(new scala.util.Random) should fullyMatch regex s"""[$alphabet]*"""
      }
    }
  }
  "The randomJavaIdent() method" should "generate strings of the correct length" in {
    forAll {
      (length: Int) => whenever (length > 0) {
        util.randomJavaIdent(length)(new scala.util.Random).length == length
      }
    }
  }
  it should "generate only valid Java identifiers" in {
    forAll {
      (length: Int) => whenever (length > 0) {
        val ident = util.randomJavaIdent(length)(new scala.util.Random)
        Character isJavaIdentifierStart ( ident charAt 0 ) shouldBe true
        ident dropRight 1 foreach {
          Character isJavaIdentifierPart _ shouldBe true
        }
      }
    }
  }
}
