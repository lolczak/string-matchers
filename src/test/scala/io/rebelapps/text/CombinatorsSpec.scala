package io.rebelapps.text

import cats.implicits._
import io.rebelapps.text.Patterns._
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}
import shapeless._

import scala.language.reflectiveCalls

class CombinatorsSpec extends FeatureSpec with SpecMatchers {

  info("Available combinators with examples")

  feature("At least once repetition") {

    val Pattern = txt("ab").+ asMatcher

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

  feature("Repetition till some pattern") {

    val Pattern = (repTill(w, txt("a")).<+> ~ w.+.<+>).tupled.matcher

    scenario("correct string") {
      "cdwrtabc" match {
        case Pattern(x, y) =>
          x shouldBe "cdwrt"
          y shouldBe "abc"
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "cdwrt" match {
          case Pattern(x, y) => fail()
        }
      }
    }
  }

  feature("0-n repetitions matcher") {

    val Pattern = txt("ab").* asMatcher

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

    val Pattern = txt("aa") ~ txt("bb") asMatcher

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
      val Pattern = (txt("aa") <~ txt("bb")).asMatcher
      "aabb" match {
        case Pattern(x) => x._1 shouldBe "aa"
      }
    }

  }

  feature("Right sequential composition") {

    scenario("correct string") {
      val Pattern = (txt("aa") ~> txt("bb")).asMatcher
      "aabb" match {
        case Pattern(x) => x._1 shouldBe "bb"
      }
    }

  }

  feature("Optional matching") {

    val Pattern = txt("aa") ~ txt("bb").? asMatcher

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

  feature("Alternative matching") {

    val Pattern = (txt("abcd") | txt("1234")).tupled.matcher

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

  feature("Alternative matching of conformant types") {

    val Pattern = (txt("abcd") || txt("1234")).tupled.matcher

    scenario("left") {
      "abcd" match {
        case Pattern(x) => x shouldBe "abcd" :: HNil
      }
    }

    scenario("right") {
      "1234" match {
        case Pattern(x) => x shouldBe "1234" :: HNil
      }
    }
  }

  feature("Alternative matching of conformant single types") {

    val Pattern = (txt("abcd") or txt("1234")).tupled.matcher

    scenario("left") {
      "abcd" match {
        case Pattern(x) => x shouldBe "abcd"
      }
    }

    scenario("right") {
      "1234" match {
        case Pattern(x) => x shouldBe "1234"
      }
    }
  }

  feature("Concatenation combinator") {

    val Pattern = con(rep1(txt("ab"))).tupled.matcher

    scenario("correct string") {
      "ababab" match {
        case Pattern(x) => x shouldBe "ababab"
      }
    }

  }

  feature("guard combinator") {

    val Pattern = (txt("aa") ~ guard(txt("ab")) ~ con(w.+)).tupled.matcher

    scenario("correct string") {
      "aaab" match {
        case Pattern(x, y) =>
          x shouldBe "aa"
          y shouldBe "ab"
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "aabb" match {
          case Pattern(x, y) => fail()
        }
      }
    }
  }

  feature("range combinator") {

    val Pattern = (con(range('a', 'z').+) ~ con(range('A', 'Z').+)).tupled.matcher

    scenario("correct string") {
      "azAZ" match {
        case Pattern(x, y) =>
          x shouldBe "az"
          y shouldBe "AZ"
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "AZaz" match {
          case Pattern(x, y) => fail()
        }
      }
    }

  }

  feature("Negation combinator") {

    val Pattern = (txt("aa") ~ Patterns.not(txt("ab")) ~ con(w.+)).tupled.matcher

    scenario("correct string") {
      "aabb" match {
        case Pattern(x, y) =>
          x shouldBe "aa"
          y shouldBe "bb"
      }
    }

    scenario("incorrect string") {
      intercept[MatchError] {
        "aaab" match {
          case Pattern(x, y) => fail()
        }
      }
    }

  }

}
