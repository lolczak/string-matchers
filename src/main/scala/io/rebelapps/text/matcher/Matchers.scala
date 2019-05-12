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

  lazy val ch = (char: Char) =>
    Matcher {
      case head :: tail if head == char => Match(char.toString, tail)
      case input                        => NoMatch(input)
    }

  lazy val txt = (const: String) =>
    Matcher { input =>
      if (input.startsWith(const.toSeq)) {
        Match(List(const), input.drop(const.length))
      } else {
        NoMatch(input)
      }
    }

//  val word: Matcher = ???
//
//  val alpha: Matcher = ???
//
//  val digit: Matcher = ???

  val opt = (m: Matcher) =>
    Matcher { input =>
      m(input) match {
        case NoMatch(next) => Match(Nil, next)
        case other         => other
      }
    }

      //rep

  //repN

  lazy val whitespace: Matcher =
    Matcher {
      case head :: tail if head.isWhitespace => Match(head.toString, tail)
      case input                             => NoMatch(input)
    }

  lazy val whitespaces: Matcher = rep1(whitespace)

}
