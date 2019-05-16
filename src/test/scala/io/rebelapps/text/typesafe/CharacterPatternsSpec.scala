package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Patterns._
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
          case Pattern(x) => fail()
        }
      }
    }

  }

  feature("whitespace matcher") {

    val Pattern = space.compile

    scenario("match") {
      " " match {
        case Pattern(x) => x._1 shouldBe " "
      }
      "\t" match {
        case Pattern(x) => x._1 shouldBe "\t"
      }
    }

    scenario("no match") {
      intercept[MatchError] {
        "b" match {
          case Pattern(x) => fail()
        }
      }
    }

  }

}

case object A
case object B
