package io.rebelapps.text.matcher

import org.scalatest.{FlatSpec, Matchers => SpecMatchers}

class MatchersSpec extends FlatSpec with SpecMatchers {
  it should "work" in {
    import io.rebelapps.text.matcher.Matchers._
    val Pattern = (txt("aa") <~ whitespaces) ~ txt("bb") ~ ch('c').+ <~ whitespaces
    "aa    bbcc    " match {
      case Pattern(a, b, c) =>
        println(a)
        println(b)
        println(c)
        a shouldBe "aa"
        b shouldBe "bb"
        c shouldBe "cc"
    }

  }
}
