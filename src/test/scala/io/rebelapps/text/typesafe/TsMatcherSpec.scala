package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.TsMatcher.acceptChar
import org.scalatest.{FlatSpec, Matchers}
import scala.language.reflectiveCalls

class TsMatcherSpec extends FlatSpec with Matchers {

  "A type safe matcher" should "work" in {
    val Pattern =
      (acceptChar(_ == 'a') ~ acceptChar(_ == 'b') <~ acceptChar(_ == 'c')).compile.it

    "abc" match {
      case Pattern(x,y) =>
        println(x)
        println(y)
    }

  }

}
