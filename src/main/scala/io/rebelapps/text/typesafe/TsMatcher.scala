package io.rebelapps.text.typesafe

import shapeless.ops.hlist.{Prepend, Tupler}
import shapeless.{::, HList, HNil}

abstract class TsMatcher[A <: HList] extends (List[Char] => TsMatcherResult[A]) {

  override def apply(input: List[Char]): TsMatcherResult[A]

  def ~[B <: HList](next: TsMatcher[B])(implicit prepend : Prepend[A, B]) : TsMatcher[prepend.Out] =
    TsMatcher { input =>
      this.apply(input) match {
        case TsMatch(terms, rest) => next(rest).mapMatches[prepend.Out](suffix => terms.++(suffix)(prepend))
        case _                    => TsNoMatch[prepend.Out](input)
      }
    }

  def <~[B <: HList](next: TsMatcher[B]): TsMatcher[A] =
    TsMatcher { input =>
      this.apply(input) match {
        case TsMatch(terms, rest) => next(rest) mapMatches[A] (_ => terms)
        case _                    => TsNoMatch[A](input)
      }
    }

  def unapplySeq(input: String)(implicit tupler : Tupler[A]): Option[tupler.Out] = {
    apply(input.toList) match {
      case TsMatch(matches, Nil) => Some(matches.tupled(tupler))
      case _                     => None
    }
  }

  def compile(implicit tupler : Tupler[A]): MatcherExtractor[tupler.Out] = {

    new MatcherExtractor[tupler.Out]({ input: String =>
      apply(input.toList) match {
        case TsMatch(matches, Nil) => Some(matches.tupled(tupler))
        case _                     => None
      }
    })
  }

}

class MatcherExtractor[A](f: String => Option[A]) {
  import shapeless.syntax.std.tuple._
  def unapplySeq(input: String): Option[(A, Seq[Nothing])] = f(input).map(_ -> Seq.empty)

}

object TsMatcher {

  def apply[A <: HList](f: List[Char] => TsMatcherResult[A]): TsMatcher[A] =
    new TsMatcher[A] { def apply(input: List[Char]): TsMatcherResult[A] = f(input) }

  lazy val acceptChar = (f: Char => Boolean) =>
    TsMatcher[String :: HNil] {
      case input if f(input.head)  => TsMatch[String :: HNil](input.head.toString :: HNil, input.tail)
      case input                   => TsNoMatch[String :: HNil](input)
    }

}
