package io.rebelapps.text.typesafe

import shapeless.ops.hlist.{Prepend, Tupler}
import shapeless.{::, HList, HNil}

abstract class Pattern[A <: HList] extends (List[Char] => TsMatcherResult[A]) {
  self =>

  override def apply(input: List[Char]): TsMatcherResult[A]

  def map[B, C](f: B => C)(implicit ev: A <:< (B :: HNil)): Pattern[C :: HNil] =
    Pattern { input =>
      this.apply(input) match {
        case TsMatch(matches, rest) => TsMatch(f(matches.head) :: HNil, rest)
        case _ => TsNoMatch[C :: HNil](input)
      }
    }

  def ~[B <: HList](next: Pattern[B])(implicit prepend: Prepend[A, B]): Pattern[prepend.Out] =
    Pattern { input =>
      this.apply(input) match {
        case TsMatch(terms, rest) => next(rest).mapMatches[prepend.Out](suffix => terms.++(suffix)(prepend))
        case _ => TsNoMatch[prepend.Out](input)
      }
    }

  def <~[B <: HList](next: Pattern[B]): Pattern[A] =
    Pattern { input =>
      this.apply(input) match {
        case TsMatch(terms, rest) => next(rest) mapMatches[A] (_ => terms)
        case _ => TsNoMatch[A](input)
      }
    }

  def compile(implicit tupler: Tupler[A]): Matcher[tupler.Out] =
    new Matcher[tupler.Out]({ input: String =>
      apply(input.toList) match {
        case TsMatch(matches, Nil) => Some(matches.tupled(tupler))
        case _ => None
      }
    })
}

class Matcher[A](f: String => Option[A]) {

  def unapplySeq(input: String): Option[(A, Seq[Nothing])] = f(input).map(_ -> Seq.empty)

}

object Pattern {

  def apply[A <: HList](f: List[Char] => TsMatcherResult[A]): Pattern[A] =
    new Pattern[A] { def apply(input: List[Char]): TsMatcherResult[A] = f(input) }

  lazy val acceptChar = (f: Char => Boolean) =>
    Pattern[String :: HNil] {
      case input if f(input.head)  => TsMatch[String :: HNil](input.head.toString :: HNil, input.tail)
      case input                   => TsNoMatch[String :: HNil](input)
    }

}
