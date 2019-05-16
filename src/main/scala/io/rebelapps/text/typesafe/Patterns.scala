package io.rebelapps.text.typesafe

import shapeless.{::, HNil}

object Patterns {

  lazy val acceptChar = (f: Char => Boolean) =>
    Pattern[String :: HNil] {
      case input if f(input.head) => TsMatch[String :: HNil](input.head.toString :: HNil, input.tail)
      case input                  => TsNoMatch[String :: HNil](input)
    }

}
