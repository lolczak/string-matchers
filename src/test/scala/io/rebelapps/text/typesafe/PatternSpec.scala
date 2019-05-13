package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Pattern.acceptChar
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class PatternSpec extends FlatSpec with Matchers {

  "A type safe matcher" should "work" in {
    case object A
    case object B

    val Pattern: MatcherExtractor[(A.type, String, Seq[Nothing])] =
      (acceptChar(_ == 'a').map((_:String) => A) ~ acceptChar(_ == 'b') <~ acceptChar(_ == 'c')).compile.it

    "abc" match {
      case Pattern(xx,yy) =>
        println(xx)
        println(yy)
    }

  }

}
