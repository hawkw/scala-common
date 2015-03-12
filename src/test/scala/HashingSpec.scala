import java.security.MessageDigest

import me.hawkweisman.common.authtools
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
/**
 * Created by hawk on 3/12/15.
 */
class HashingSpec extends FlatSpec with GeneratorDrivenPropertyChecks{

  "The hash() method" should "generate correct SHA-512 hashes" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest.getInstance("SHA-512")
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        authtools.hash(pass, salt = salt) ==(hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
  it should "generate correct SHA-512 hashes when passed SHA-512 explicitly" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest.getInstance("SHA-512")
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        authtools.hash(pass, salt = salt, algorithm="SHA-512") ==(hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
  it should "generate correct SHA-256 hashes" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest.getInstance("SHA-256")
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        authtools.hash(pass, salt = salt, algorithm="SHA-256") ==(hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
}
