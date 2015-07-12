package me.hawkweisman.util

import java.security.MessageDigest
import java.security.NoSuchAlgorithmException

import scala.language.postfixOps

/**
 * ==Auth==
 *
 * Utilities for working with authorization. The primary element of this
 * package is the [[hash]] method, which can be used for hashing new
 * passwords, or validating an existing password against a known hash.
 *
 * ==Examples==
 *
 * ====Hashing new passwords====
 * Assume we have a string called `password` containing a new password.
 * {{{
 * val (hashedPass, salt) = hash(password)
 * // the hash and the salt can now be inserted into the database
 * }}}
 *
 * Using a different hashing algorithm:
 *
 * The passed algorithm can be  any hashing algorithm in
 * [[java.security.MessageDigest]].
 *
 * {{{
 * val (hashedPass, salt) = hash(password, "MD5")
 * // note: please don't actually use MD5 in production
 * }}}
 *
 *
 * Using a different salt source:
 *
 * {{{
 * val (hashedPass, salt) = hash(
 *   password,
 *   saltSource = randomAlphanumericString(256)
 * )
 * }}}
 *
 * ====Validating extant passwords====
 * Assume that we have already gotten the password from the user,
 * and that we have gotten the salt and the expected hash from the
 * database (stored in the variables `salt` and `expectedHash`).
 * {{{
 * val (digest, _) = hash(password, saltSource = salt)) )
 * if (expectedHash == digest) {
 *    // ... do stuff requiring auth
 * }
 * }}}
 *
 * @author Hawk Weisman
 */
object auth {

  val defaultHashLength = 16

  /**
   * General-purpose password hashing method.
   *
   * This can be used for generating hashes for new passwords or for validating
   * existing passwords, depending on the passed arguments.
   *
   *
   * @param pass The password to hash.
   * @param algorithm The hashing algorithm. Defaults to SHA-512.
   * @param salt The salt
   * @throws NoSuchAlgorithmException if the specified hashing algorithm doesn't exist
   * @return A tuple containing (hash, salt) as strings
   */
  @throws[NoSuchAlgorithmException]("if the specified hashing algorithm doesn't exist")
  def hash(
   pass: String,
   algorithm: String = "SHA-512",
   salt: String = random.randomAlphanumericString(defaultHashLength)(new scala.util.Random)
   ): (String,String) = {
    val hasher = MessageDigest.getInstance(algorithm)
    hasher update ( pass getBytes )
    hasher update ( salt getBytes )
    (hasher.digest.map(Integer.toHexString(_)).mkString, salt)
  }

}
