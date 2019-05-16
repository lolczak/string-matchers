package io.rebelapps.text.typesafe

import shapeless.{::, HNil}

object Patterns {

  lazy val acceptChar = (f: Char => Boolean) =>
    Pattern[String :: HNil] {
      case input if f(input.head) => TsMatch[String :: HNil](input.head.toString :: HNil, input.tail)
      case input                  => TsNoMatch[String :: HNil](input)
    }

  lazy val space = acceptChar(_.isWhitespace)

  lazy val digit = acceptChar(_.isDigit)

  lazy val word = acceptChar(ch => ch.isLetterOrDigit || ch == '_')

  lazy val alpha = acceptChar(_.isLetter)

  lazy val txt = (const: String) =>
    Pattern { input =>
      if (input.startsWith(const.toSeq)) {
        TsMatch[String :: HNil](const :: HNil, input.drop(const.length))
      } else {
        TsNoMatch[String :: HNil](input)
      }
    }

}
