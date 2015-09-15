package me.hawkweisman.util
package tests

import java.security.MessageDigest

import auth.hash
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.language.postfixOps

/**
 * Created by hawk on 3/12/15.
 */
class AuthSpec
extends FlatSpec
  with GeneratorDrivenPropertyChecks
  with Matchers {

  val sha512 = "SHA-512"
  val sha256 = "SHA-256"

  "The hash() method" should "generate correct SHA-512 hashes" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest getInstance sha512
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        hash(pass, salt = salt) ==(hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
  it should "generate correct SHA-512 hashes when passed SHA-512 explicitly" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest getInstance sha512
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        hash(pass, salt = salt, algorithm=sha512) == (hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
  it should "generate correct SHA-256 hashes" in {
    forAll {
      (pass: String, salt: String) => whenever(pass.length > 0 && salt.length() > 0) {
        val hasher = MessageDigest getInstance sha256
        hasher update (pass getBytes)
        hasher update (salt getBytes)
        hash(pass, salt = salt, algorithm=sha256) == (hasher.digest.map(Integer.toHexString(_)).mkString, salt)
      }
    }
  }
 }
