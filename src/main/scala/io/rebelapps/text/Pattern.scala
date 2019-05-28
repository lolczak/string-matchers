package io.rebelapps.text

import io.rebelapps.text
import io.rebelapps.text.Patterns.{opt, rep0, rep1}
import shapeless.ops.hlist.{Prepend, Tupler}
import shapeless.{::, HList, HNil}

abstract class Pattern[A <: HList] extends (List[Char] => MatcherResult[A]) {
  self =>

  override def apply(input: List[Char]): MatcherResult[A]

  def ^^[B, C](f: B => C)(implicit ev: A <:< (B :: HNil)): Pattern[C :: HNil] = mapSingle(f)

  def ^^^[B, C](result: => C)(implicit ev: A <:< (B :: HNil)): Pattern[C :: HNil] = mapSingle[B, C](_ => result)

  def mapSingle[B, C](f: B => C)(implicit ev: A <:< (B :: HNil)): Pattern[C :: HNil] =
    Pattern { input =>
      this.apply(input) match {
        case Match(matches, rest) => Match(f(matches.head) :: HNil, rest)
        case _                    => NoMatch[C :: HNil](input)
      }
    }

  def <>[B <: HList](f: A => B): Pattern[B] = map(f)

  def map[B <: HList](f: A => B): Pattern[B] =
    Pattern { input =>
      this.apply(input) match {
        case Match(matches, rest) => Match(f(matches), rest)
        case _                      => NoMatch[B](input)
      }
    }

  lazy val ? = opt(self)

  lazy val + = rep1(self)

  lazy val * = rep0(self)

  def unapplySeq(input: String): Option[(A, Seq[Nothing])] =
    self(input.toList) match {
      case Match(matches, Nil) => Some(matches -> Seq.empty)
      case _                     => None
    }

  def asMatcher(implicit tupler: Tupler[A]): Matcher[tupler.Out] =
    new Matcher[tupler.Out]({ input: String =>
      apply(input.toList) match {
        case Match(matches, Nil) => Some(matches.tupled(tupler))
        case _                   => None
      }
    })

  def tupled(implicit prepend: Prepend[A, Seq[Nothing] :: HNil]) = new {

    private val newPattern: Pattern[prepend.Out] = text.Pattern { input =>
      self.apply(input) match {
        case Match(matches, rest)  => Match(matches.:+(Seq.empty)(prepend), rest)
        case _                     => NoMatch[prepend.Out](input)
      }
    }

    def matcher(implicit tupler : Tupler[prepend.Out]): MatcherExtractor[tupler.Out] = {

      new MatcherExtractor[tupler.Out]({ input: String =>
        newPattern.apply(input.toList) match {
          case Match(matches, Nil) => Some(matches.tupled(tupler))
          case _                   => None
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

  def apply[A <: HList](f: List[Char] => MatcherResult[A]): Pattern[A] =
    new Pattern[A] { def apply(input: List[Char]): MatcherResult[A] = f(input) }

  implicit def patternOps[A <: HList](p: Pattern[A]): PatternOps[A] = new PatternOps[A](p)

}
