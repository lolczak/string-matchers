package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.TsMatcher.acceptChar
import org.scalatest.{FlatSpec, Matchers}

class TsMatcherSpec extends FlatSpec with Matchers {

  "A type safe matcher" should "work" in {
    val Pattern: MatcherExtractor[(String, String)] =
      (acceptChar(_ == 'a') ~ acceptChar(_ == 'b') <~ acceptChar(_ == 'c')).compile

    "abc" match {
      case Pattern((x,y)) =>
        println(x)
        println(y)
    }

  }

}
