import java.security.MessageDigest

import me.hawkweisman.util.auth
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
/**
 * Created by hawk on 3/12/15.
 */
class AuthSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  "The hash() method" should "generate correct SHA-512 hashes" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest.getInstance("SHA-512")
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        auth.hash(pass, salt = salt) ==(hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
  it should "generate correct SHA-512 hashes when passed SHA-512 explicitly" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest.getInstance("SHA-512")
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        auth.hash(pass, salt = salt, algorithm="SHA-512") ==(hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
  it should "generate correct SHA-256 hashes" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest.getInstance("SHA-256")
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        auth.hash(pass, salt = salt, algorithm="SHA-256") ==(hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
  "The randomString() method" should "generate strings of the correct length" in {
    forAll {
      (length: Int) => whenever (length > 0) {
        auth.randomAlphanumericString(length)(new scala.util.Random).length == length
      }
    }
  }
  it should "generate strings in the alphabet [abcdefghijklmnopqrstuvwxyz0123456789]*" in {
    forAll {
      (length: Int) => whenever (length > 0) {
        auth.randomAlphanumericString(length)(new scala.util.Random) should fullyMatch regex """[abcdefghijklmnopqrstuvwxyz0123456789]*"""
      }
    }
  }
  "The randomAlphanumericString() method" should "generate strings of the correct length" in {
    forAll {
      (alphabet: String, length: Int) => whenever (alphabet != "" && length > 0) {
        auth.randomString(alphabet)(length)(new scala.util.Random).length == length
      }
    }
  }
  it should "generate strings in the specified alphabet" in {
    forAll {
      (alphabet: String, length: Int) => whenever (alphabet != "" && length > 0) {
        auth.randomString(alphabet)(length)(new scala.util.Random) should fullyMatch regex s"""[$alphabet]*"""
      }
    }
  }
 }
