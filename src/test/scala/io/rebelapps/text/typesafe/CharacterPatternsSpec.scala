package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Pattern.acceptChar
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}

class CharacterPatternsSpec extends FeatureSpec with SpecMatchers {

  info("Character class patterns")

  feature("arbitrary character matcher") {

    val Pattern = acceptChar(_ == 'a').compile

    scenario("match") {
      "a" match {
        case Pattern(x) => x._1 shouldBe "a"
      }
    }

    scenario("no match") {
      intercept[MatchError] {
        "b" match {
          case Pattern(x) => x shouldBe "a"
        }
      }
    }

  }

}

case object A
case object B
