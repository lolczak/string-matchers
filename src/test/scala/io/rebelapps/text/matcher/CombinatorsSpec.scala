package io.rebelapps.text.matcher

import io.rebelapps.text.matcher.Matchers._
import org.scalatest.{FeatureSpec, Matchers => SpecMatchers}

class CombinatorsSpec extends FeatureSpec with SpecMatchers {

  info("Available combinators with examples")

  feature("Sequential composition") {
    scenario("correct string") {
      val Pattern = txt("aa") ~ txt("bb")
      "aabb" match {
        case Pattern(x, y) =>
          x shouldBe "aa"
          y shouldBe "bb"
      }

    }
  }

}
