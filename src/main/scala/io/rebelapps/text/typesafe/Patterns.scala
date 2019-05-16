package io.rebelapps.text.typesafe

import shapeless.{::, HList, HNil}

import scala.annotation.tailrec

object Patterns {

  lazy val acceptChar = (f: Char => Boolean) =>
    Pattern[String :: HNil] {
      case input if f(input.head) => TsMatch[String :: HNil](input.head.toString :: HNil, input.tail)
      case input                  => TsNoMatch[String :: HNil](input)
    }

  lazy val ch = (ch: Char) => acceptChar(_ == ch)

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

  private lazy val punctuationCharacters = """][!"#$%&'()*+,./:;<=>?@\^_`{|}~-""".toCharArray.toSet

  lazy val punct = acceptChar(punctuationCharacters.contains)

  def rep1[A <: HList](p: Pattern[A]):Pattern[List[A] :: HNil] =
    Pattern { next =>
      @tailrec
      def loop(next: List[Char], acc: List[A] = List.empty): TsMatcherResult[List[A] :: HNil] = {
        p(next) match {
          case TsNoMatch(_) if acc.isEmpty  => TsNoMatch[List[A] :: HNil](next)
          case TsNoMatch(_) if acc.nonEmpty => TsMatch[List[A] :: HNil](acc :: HNil, next)
          case TsMatch(t, n)                => loop(n, acc :+ t)
        }
      }

      loop(next)
    }

}
