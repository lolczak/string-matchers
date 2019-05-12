package io.rebelapps.text.matcher
import io.rebelapps.text.matcher.Matchers._
import org.scalatest.{FlatSpec, Matchers => SpecMatchers}

class MatchersSpec extends FlatSpec with SpecMatchers {

  "String matchers" should "work" in {
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

  it should "return left matches" in {
    val Pattern = txt("aa") <~ txt("bb")
    "aabb" match {
      case Pattern(x) => x shouldBe "aa"
    }
  }

  it should "return right matches" in {
    val Pattern = txt("aa") ~> txt("bb")
    "aabb" match {
      case Pattern(x) => x shouldBe "bb"
    }
  }

  it should "optionally match pattern" in {
    val Pattern = txt("aa") ~ txt("bb").?
    "aabb" match {
      case Pattern(x, y) =>
        x shouldBe "aa"
        y shouldBe "bb"
    }
    "aa" match {
      case Pattern(x) => x shouldBe "aa"
    }
  }

  it should "match alphanumeric characters" in {
    val Pattern = word.+
    "abcCxłó12_" match {
      case Pattern(x) => x shouldBe "abcCxłó12_"
    }
    intercept[MatchError] {
      "abcCx   łó12_" match {
        case Pattern(x) => fail()
      }
    }
  }

}
