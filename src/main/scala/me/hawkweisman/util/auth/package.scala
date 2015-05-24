package me.hawkweisman.util

import java.security.NoSuchAlgorithmException
import java.security.MessageDigest
import scala.language.postfixOps

/**
 * Auth stuff
 * @author Hawk Weisman <hi@hawkweisman.me>
 */
package object auth {
  
  val defaultHashLength = 16

  /**
   * General-purpose password hashing method. This can be used for generating hashes
   * for new passwords or for validating existing passwords, depending on the passed arguments.
   *
   * Examples:
   * ---------
   *
   * ### Hashing new passwords
   *
   * ```scala
   * # assume we have a string called "password" containing a new password
   * val (digest, salt) = hash(password)
   * # the hash and the salt can now be inserted into the database
   * ```
   *
   * Using a different hashing algorithm:
   *
   * ```scala
   * val (digest, salt) = hash(password, "MD5") # please don't actually use MD5
   * ```
   *
   * Using a different salt source:
   *
   * ```scala
   * val (digest, salt) = hash(password, saltSource = randomAlphanumericString(256))
   * ```
   *
   * ### Validating extant passwords:
   *
   * ```scala
   * # assume we have already gotten the password from the user
   * # and the salt and hash from the DB ('salt' and 'expectedDigest')
   * val (digest, _) = hash(password, saltSource = salt)) )
   * if (expectedDigest == digest) {
   *    # ... do stuff requiring auth
   * }
   * ```
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
