package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Patterns._
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}
import shapeless._

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

//  feature("Right sequential composition") {
//
//    scenario("correct string") {
//      val Pattern = txt("aa") ~> txt("bb")
//      "aabb" match {
//        case Pattern(x) => x shouldBe "bb"
//      }
//    }
//
//  }

}
