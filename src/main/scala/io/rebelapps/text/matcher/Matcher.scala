package io.rebelapps.text.matcher

import io.rebelapps.text.matcher.Matchers.rep1

abstract class Matcher extends (List[Char] => MatcherResult) {

  def apply(input: List[Char]): MatcherResult

  lazy val + = rep1(this)

  def ~(next: Matcher): Matcher =
    Matcher { input =>
      this.apply(input) match {
        case Match(terms, rest) => next(rest) mapMatches (terms ++ _)
        case other              => other
      }
    }

  def <~(next: Matcher): Matcher =
    Matcher { input =>
      this.apply(input) match {
        case Match(terms, rest) => next(rest) mapMatches (_ => terms)
        case other              => other
      }
    }

  def ~>(next: Matcher): Matcher = ???

  def unapplySeq(input: String): Option[List[String]] = {
    apply(input.toList) match {
      case Match(terms, Nil) => Some(terms)
      case other             => None
    }
  }

}

object Matcher {

  def apply(f: List[Char] => MatcherResult): Matcher = new Matcher {def apply(input: List[Char]): MatcherResult = f(input)}

}
