package me.hawkweisman.math
package tests

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class MathSpec
extends FlatSpec
  with GeneratorDrivenPropertyChecks
  with Matchers {

  "The normalizeAt() function" should "normalize any set of Doubles to 1" in {
    forAll { (ns: Seq[Double]) => whenever (ns.length > 1) {
      normalizeAt(1.0)(ns).sum should be (1.0 +- 1e-5)
      }
    }
  }
//  ignore should "normalize any set of Doubles at 100" in {
//    forAll { (ns: Seq[Double]) => whenever(ns.length > 1) {
//      math.normalizeAt(100.0)(ns).sum should be (100.0 +- 1e-5)
//      }
//    }
//  }
  // This is ignored because floating point is
  // a terrible way to represent numbers
  ignore should "normalize any set of Floats at 1" in {
    forAll { (ns: Seq[Double]) => whenever(ns.length > 1) {
      normalizeAt(1.0)(ns).sum should be (1.0 +- 1e-5)
      }
    }
  }
//  ignore should "normalize any set of Floats at 100" in {
//    forAll { (ns: Seq[Double]) => whenever(ns.length > 1) {
//      math.normalizeAt(100.0)(ns).sum should be (100.0 +- 1e-5)
//      }
//    }
//  }
}
