package io.rebelapps.text.matcher

import scala.annotation.tailrec

object Matchers {

  lazy val rep1 = (m: Matcher) =>
    Matcher { next =>
      @tailrec
      def loop(next: List[Char], acc: List[String] = List.empty): MatcherResult = {
        m(next) match {
          case NoMatch(_) if acc.isEmpty  => NoMatch(next)
          case NoMatch(_) if acc.nonEmpty => Match(acc, next)
          case Match(t, n)                => loop(n, acc ++ t)
        }
      }

      loop(next) mapMatches(m => List(m.mkString))
    }

  //rep

  lazy val acceptChar = (f: Char => Boolean) =>
    Matcher {
      case head :: tail if f(head) => Match(head.toString, tail)
      case input                   => NoMatch(input)
    }

  lazy val ch = (ch: Char) => acceptChar(_ == ch)

  lazy val txt = (const: String) =>
    Matcher { input =>
      if (input.startsWith(const.toSeq)) {
        Match(List(const), input.drop(const.length))
      } else {
        NoMatch(input)
      }
    }

  lazy val word = acceptChar(ch => ch.isLetterOrDigit || ch == '_')

  lazy val alpha: Matcher = acceptChar(_.isLetter)

  lazy val digit: Matcher = acceptChar(_.isDigit)

  lazy val opt = (m: Matcher) =>
    Matcher { input =>
      m(input) match {
        case NoMatch(next) => Match(Nil, next)
        case other         => other
      }
    }

  lazy val con = (m: Matcher) => Matcher { input => m(input) mapMatches (m => List(m.mkString)) }

  lazy val alt = (left: Matcher) => (right: Matcher) =>
    Matcher { input =>
      left(input) match {
        case NoMatch(_) => right(input)
        case other      => other
      }
    }

  lazy val whitespace: Matcher =
    Matcher {
      case head :: tail if head.isWhitespace => Match(head.toString, tail)
      case input                             => NoMatch(input)
    }

  lazy val whitespaces: Matcher = rep1(whitespace)

}
