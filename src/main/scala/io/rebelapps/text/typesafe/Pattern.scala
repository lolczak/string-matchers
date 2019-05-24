package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Patterns.{alt, opt, rep0, rep1}
import shapeless.ops.hlist.{Prepend, Tupler}
import shapeless.{::, HList, HNil}

abstract class Pattern[A <: HList] extends (List[Char] => TsMatcherResult[A]) {
  self =>

  override def apply(input: List[Char]): TsMatcherResult[A]

  def ^^[B, C](f: B => C)(implicit ev: A <:< (B :: HNil)): Pattern[C :: HNil] = map(f)

  def map[B, C](f: B => C)(implicit ev: A <:< (B :: HNil)): Pattern[C :: HNil] =
    Pattern { input =>
      this.apply(input) match {
        case TsMatch(matches, rest) => TsMatch(f(matches.head) :: HNil, rest)
        case _                      => TsNoMatch[C :: HNil](input)
      }
    }

  lazy val ? = opt(self)

  lazy val + = rep1(self)

  lazy val * = rep0(self)

  def unapplySeq(input: String): Option[(A, Seq[Nothing])] =
    self(input.toList) match {
      case TsMatch(matches, Nil) => Some(matches -> Seq.empty)
      case _                     => None
    }

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
        case TsMatch(matches, rest) => TsMatch(matches.:+(Seq.empty)(prepend), rest)
        case _                      => TsNoMatch[prepend.Out](input)
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

  implicit def patternOps[A <: HList](p: Pattern[A]): PatternOps[A] = new PatternOps[A](p)

}
