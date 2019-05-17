package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Patterns._
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}
import shapeless._

import scala.language.reflectiveCalls

class CombinatorsSpec extends FeatureSpec with SpecMatchers {

  info("Available combinators with examples")

  feature("At least once repetition") {

    val Pattern = rep1(txt("ab")).compile

    scenario("correct string") {
      "ababab" match {
        case Pattern(x) => x._1 shouldBe List("ab" :: HNil, "ab" :: HNil, "ab" :: HNil)
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "aacbb" match {
          case Pattern(x) => fail()
        }
      }
    }
  }

  feature("0-n repetitions matcher") {

    val Pattern = rep0(txt("ab")).compile

    scenario("correct string") {
      "ab" match {
        case Pattern(x) => x._1 shouldBe List("ab" :: HNil)
      }
      "" match {
        case Pattern(x) => x._1 shouldBe List.empty
      }
      "ababab" match {
        case Pattern(x) => x._1 shouldBe List("ab" :: HNil, "ab" :: HNil, "ab" :: HNil)
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "aacbb" match {
          case Pattern(x) => fail()
        }
      }
    }
  }

  feature("Sequential composition") {

    val Pattern = txt("aa") ~ txt("bb") compile

    scenario("correct string") {
      "aabb" match {
        case Pattern((x, y)) =>
          x shouldBe "aa"
          y shouldBe "bb"
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "aacbb" match {
          case Pattern((x, y)) => fail()
        }
      }
    }
  }

  feature("Left sequential composition") {

    scenario("correct string") {
      val Pattern = (txt("aa") <~ txt("bb")).compile
      "aabb" match {
        case Pattern(x) => x._1 shouldBe "aa"
      }
    }

  }

  feature("Right sequential composition") {

    scenario("correct string") {
      val Pattern = (txt("aa") ~> txt("bb")).compile
      "aabb" match {
        case Pattern(x) => x._1 shouldBe "bb"
      }
    }

  }

  feature("Optional matching") {

    val Pattern = txt("aa") ~ opt(txt("bb")) compile

    scenario("some") {
      "aabb" match {
        case Pattern((x, y)) =>
          x shouldBe "aa"
          y shouldBe Some("bb" :: HNil)
      }
    }

    scenario("none") {
      "aa" match {
        case Pattern((x, y)) =>
          x shouldBe "aa"
          y shouldBe None
      }
    }
  }

  feature("Alternative syntax") {

    val Pattern = (txt("aa") ~ opt(txt("bb"))).interpret.it

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

  feature("Alternative matching") {

    val Pattern = alt(txt("abcd"))(txt("1234")).interpret.it

    scenario("left") {
      "abcd" match {
        case Pattern(x) => x shouldBe Left("abcd" :: HNil)
      }
    }

    scenario("right") {
      "1234" match {
        case Pattern(x) => x shouldBe Right("1234" :: HNil)
      }
    }
  }

}
