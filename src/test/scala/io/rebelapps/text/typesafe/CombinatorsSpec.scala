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

}
