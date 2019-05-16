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

  feature("digit matcher") {

    val Pattern = digit.compile

    scenario("match") {
      "1" match {
        case Pattern(x) => x._1 shouldBe "1"
      }
      "9" match {
        case Pattern(x) => x._1 shouldBe "9"
      }
    }

    scenario("no match") {
      intercept[MatchError] {
        "_" match {
          case Pattern(x) => fail()
        }
      }
    }

  }

  feature("word matcher") {

    val Pattern = word.compile

    scenario("match") {
      "a" match {
        case Pattern(x) => x._1 shouldBe "a"
      }
      "_" match {
        case Pattern(x) => x._1 shouldBe "_"
      }
      "9" match {
        case Pattern(x) => x._1 shouldBe "9"
      }
    }

    scenario("no match") {
      intercept[MatchError] {
        " " match {
          case Pattern(x) => fail()
        }
      }
    }

  }

  feature("alpha matcher") {

    val Pattern = alpha.compile

    scenario("match") {
      "a" match {
        case Pattern(x) => x._1 shouldBe "a"
      }
      "b" match {
        case Pattern(x) => x._1 shouldBe "b"
      }
    }

    scenario("no match") {
      intercept[MatchError] {
        "1" match {
          case Pattern(x) => fail()
        }
      }
    }

  }
  feature("text matcher") {

    val Pattern = txt("abc").compile

    scenario("match") {
      "abc" match {
        case Pattern(x) => x._1 shouldBe "abc"
      }
    }

    scenario("no match") {
      intercept[MatchError] {
        "1" match {
          case Pattern(x) => fail()
        }
      }
    }

  }

}

case object A
case object B
