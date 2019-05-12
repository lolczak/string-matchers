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

  it should "match digits" in {
    val Pattern = digit.+
    "122032498432" match {
      case Pattern(x) => x shouldBe "122032498432"
    }
    intercept[MatchError] {
      "abcCx   łó12_" match {
        case Pattern(x) => fail()
      }
    }
  }

  it should "concatenate matches" in {
    val Pattern = con(alpha.+ ~ digit.+)
    "abcd1234" match {
      case Pattern(x) => x shouldBe "abcd1234"
    }
  }

  it should "match alternative of patterns" in {
    val Pattern = alpha.+ | digit.+
    "abcd" match {
      case Pattern(x) => x shouldBe "abcd"
    }
    "1234" match {
      case Pattern(x) => x shouldBe "1234"
    }
  }

}
