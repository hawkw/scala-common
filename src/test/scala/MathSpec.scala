import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import me.hawkweisman.util.math

class MathSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  "The normalizeAt() function" should "normalize any set of Doubles to 1" in {
    forAll { (ns: Seq[Double]) => whenever (ns.length > 1) {
      math.normalizeAt(1.0)(ns).sum should be (1.0 plusOrMinus 1e-5)
      }
    }
  }/*
  it should "normalize any set of Doubles at 100" in {
    forAll { (ns: Seq[Double]) => whenever(ns.length > 1) {
      math.normalizeAt(100.0)(ns).sum should be (100.0 plusOrMinus 1e-5)
      }
    }
  }*/
  it should "normalize any set of Floats at 1" in {
    forAll { (ns: Seq[Double]) => whenever(ns.length > 1) {
      math.normalizeAt(1.0)(ns).sum should be (1.0 plusOrMinus 1e-5)
      }
    }
  }/*
  it should "normalize any set of Floats at 100" in {
    forAll { (ns: Seq[Double]) => whenever(ns.length > 1) {
      math.normalizeAt(100.0)(ns).sum should be (100.0 plusOrMinus 1e-5)
      }
    }
  }*/
}
