package io.rebelapps.text.matcher

import io.rebelapps.text.matcher.Matchers._
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}

class CombinatorsSpec extends FeatureSpec with SpecMatchers {

  info("Available combinators with examples")

  feature("Sequential composition") {

    val Pattern = txt("aa") ~ txt("bb")

    scenario("correct string") {
      "aabb" match {
        case Pattern(x, y) =>
          x shouldBe "aa"
          y shouldBe "bb"
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "aacbb" match {
          case Pattern(x, y) => fail()
        }
      }
    }
  }

  feature("Left sequential composition") {

    scenario("correct string") {
      val Pattern = txt("aa") <~ txt("bb")
      "aabb" match {
        case Pattern(x) => x shouldBe "aa"
      }
    }

  }

  feature("Right sequential composition") {

    scenario("correct string") {
      val Pattern = txt("aa") ~> txt("bb")
      "aabb" match {
        case Pattern(x) => x shouldBe "bb"
      }
    }

  }

  feature("Optional combinator") {

    val Pattern = txt("aa") ~ txt("bb").?

    scenario("some") {
      "aabb" match {
        case Pattern(x, y) =>
          x shouldBe "aa"
          y shouldBe "bb"
      }
    }

    scenario("none") {
      "aa" match {
        case Pattern(x) => x shouldBe "aa"
      }
    }
  }

}
