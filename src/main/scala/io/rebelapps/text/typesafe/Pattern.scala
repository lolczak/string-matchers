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
        case _                      => TsNoMatch[C :: HNil](input)
      }
    }

  def ~[B <: HList](next: Pattern[B])(implicit prepend: Prepend[A, B]): Pattern[prepend.Out] =
    Pattern { input =>
      this.apply(input) match {
        case TsMatch(terms, rest) => next(rest).mapMatches[prepend.Out](suffix => terms.++(suffix)(prepend))
        case _                    => TsNoMatch[prepend.Out](input)
      }
    }

  def <~[B <: HList](next: Pattern[B]): Pattern[A] =
    Pattern { input =>
      this.apply(input) match {
        case TsMatch(terms, rest) => next(rest) mapMatches[A] (_ => terms)
        case _                    => TsNoMatch[A](input)
      }
    }

  def ~>[B <: HList](next: Pattern[B]): Pattern[B] =
    Pattern { input =>
      this.apply(input) match {
        case TsMatch(terms, rest) => next(rest)
        case _                    => TsNoMatch[B](input)
      }
    }

  def |[B <: HList](right: Pattern[B]): Pattern[Either[A, B] :: HNil] = Patterns.alt(self)(right)

  def asMatcher(implicit tupler: Tupler[A]): Matcher[tupler.Out] =
    new Matcher[tupler.Out]({ input: String =>
      apply(input.toList) match {
        case TsMatch(matches, Nil) => Some(matches.tupled(tupler))
        case _ => None
      }
    })

  def tupled(implicit prepend: Prepend[A, Seq[Nothing] :: HNil]) = new {

    private val newPattern: Pattern[prepend.Out] = Pattern { input =>
      self.apply(input) match {
        case TsMatch(terms, rest) => TsMatch(terms.:+(Seq.empty)(prepend), rest)
        case _                    => TsNoMatch[prepend.Out](input)
      }
    }

    def matcher(implicit tupler : Tupler[prepend.Out]): MatcherExtractor[tupler.Out] = {

      new MatcherExtractor[tupler.Out]({ input: String =>
        newPattern.apply(input.toList) match {
          case TsMatch(matches, Nil) => Some(matches.tupled(tupler))
          case _                     => None
        }
      })
    }

  }

}

class MatcherExtractor[A](f: String => Option[A]) {

  def unapplySeq(input: String): Option[A] = f(input)

}

class Matcher[A](f: String => Option[A]) {

  def unapplySeq(input: String): Option[(A, Seq[Nothing])] = f(input).map(_ -> Seq.empty)

}

object Pattern {

  def apply[A <: HList](f: List[Char] => TsMatcherResult[A]): Pattern[A] =
    new Pattern[A] { def apply(input: List[Char]): TsMatcherResult[A] = f(input) }

}
