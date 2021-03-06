package io.rebelapps.text

import io.rebelapps.text.Patterns.{opt, txt}
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}
import shapeless._

import scala.language.reflectiveCalls

class PatternSpec extends FeatureSpec with SpecMatchers {

  info("Pattern features")

  feature("Pattern extractor syntax") {

    val Pattern = txt("aa") ~ opt(txt("bb"))

    scenario("some") {
      "aabb" match {
        case Pattern(x :: y :: HNil) =>
          x shouldBe "aa"
          y shouldBe Some("bb" :: HNil)
      }
    }

    scenario("none") {
      "aa" match {
        case Pattern(x :: y :: HNil) =>
          x shouldBe "aa"
          y shouldBe None
      }
    }
  }


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

  feature("Mapping over function") {

    val Pattern = (txt("aa").^^((_: String).length) ~ opt(txt("bb"))).tupled.matcher

    scenario("text mapping") {
      "aabb" match {
        case Pattern(x, y) =>
          x shouldBe 2
          y shouldBe Some("bb" :: HNil)
      }
    }

  }

  feature("change of a successful result into the specified value.") {

    val Pattern = ((txt("aa") ^^^ 1) ~ opt(txt("bb"))).tupled.matcher

    scenario("text mapping") {
      "aabb" match {
        case Pattern(x, y) =>
          x shouldBe 1
          y shouldBe Some("bb" :: HNil)
      }
    }

  }

}
