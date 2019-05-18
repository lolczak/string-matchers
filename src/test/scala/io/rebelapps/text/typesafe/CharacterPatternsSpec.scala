package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Patterns._
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}

class CharacterPatternsSpec extends FeatureSpec with SpecMatchers {

  info("Character class patterns")

  feature("arbitrary character matcher") {

    val Pattern = acceptChar(_ == 'a').asMatcher

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

    val Pattern = space.asMatcher

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

    val Pattern = digit.asMatcher

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

    val Pattern = word.asMatcher

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

    val Pattern = alpha.asMatcher

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

    val Pattern = txt("abc").asMatcher

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

  feature("punctuation characters matcher") {

    val Pattern = punct.asMatcher

    scenario("match") {
      val punct = """][!"#$%&'()*+,./:;<=>?@\^_`{|}~-""".toCharArray.toList

      punct foreach { char =>
        s"$char" match {
          case Pattern(x) => x._1 shouldBe s"$char"
        }
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
