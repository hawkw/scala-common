import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import me.hawkweisman.util

/**
 * Created by hawk on 5/9/15.
 */
class UtilSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  val lengths = Gen.choose(1,500) // this is so we don't spend forever generating strings

  "The randomAlphanumericString() method" should "generate strings of the correct length" in {
    forAll (lengths) { (length: Int) =>
      util.randomAlphanumericString(length)(new scala.util.Random) should have length length
    }
  }
  it should "generate strings in the alphabet [abcdefghijklmnopqrstuvwxyz0123456789]*" in {
    forAll (lengths) { (length: Int) =>
      util.randomAlphanumericString(length)(new scala.util.Random) should fullyMatch regex """[abcdefghijklmnopqrstuvwxyz0123456789]*"""
    }
  }
  "The randomString() method" should "generate strings of the correct length" in {
    forAll (arbitrary[String], lengths) { (alphabet: String, length: Int) =>
      whenever (alphabet != "") {
        util.randomString(alphabet)(length)(new scala.util.Random) should have length length
      }
    }
  }
  it should "generate strings in the specified alphabet" in {
    forAll (arbitrary[String], lengths) { (alphabet: String, length: Int) =>
      whenever (alphabet != "") {
        util.randomString(alphabet)(length)(new scala.util.Random) foreach {
          (c: Char) => alphabet should contain (c)
        }
      }
    }
  }
  "The randomJavaIdent() method" should "generate strings of the correct length" in {
    forAll (lengths) { (length: Int) =>
      util.randomJavaIdent(length)(new scala.util.Random) should have length length
    }
  }
  it should "generate only valid Java identifiers" in {
    forAll (lengths) { (length: Int) =>
      val ident = util.randomJavaIdent(length)(new scala.util.Random)
      Character isJavaIdentifierStart ( ident charAt 0 ) shouldBe true
      ident dropRight 1 foreach {
        Character isJavaIdentifierPart _ shouldBe true
      }
    }
  }
}
