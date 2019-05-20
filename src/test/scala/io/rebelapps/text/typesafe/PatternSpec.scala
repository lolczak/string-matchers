package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Patterns.{acceptChar, opt, txt}
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}
import shapeless.HNil

import scala.language.reflectiveCalls

class PatternSpec extends FeatureSpec with SpecMatchers {

  info("Pattern features")

  feature("Tupled matcher syntax") {

    val Pattern = (txt("aa") ~ opt(txt("bb"))).tupled.matcher

    scenario("some") {
      "aabb" match {
        case Pattern(x, y) =>
          x shouldBe "aa"
          y shouldBe Some("bb" :: HNil)
      }
    }

    scenario("none") {
      "aa" match {
        case Pattern(x, y) =>
          x shouldBe "aa"
          y shouldBe None
      }
    }
  }

}
