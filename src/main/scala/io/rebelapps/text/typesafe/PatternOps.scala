package io.rebelapps.text.typesafe

import io.rebelapps.text.typesafe.Patterns.alt
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList, HNil}

class PatternOps[A <: HList](self: Pattern[A]) {

  def ~[B <: HList](next: Pattern[B])(implicit prepend: Prepend[A, B]): Pattern[prepend.Out] =
    Pattern { input =>
      self.apply(input) match {
        case TsMatch(matches, rest) => next(rest).mapMatches[prepend.Out](suffix => matches.++(suffix)(prepend))
        case _                      => TsNoMatch[prepend.Out](input)
      }
    }


  def <~[B <: HList](next: Pattern[B]): Pattern[A] =
    Pattern { input =>
      self.apply(input) match {
        case TsMatch(matches, rest) => next(rest) mapMatches[A] (_ => matches)
        case _                      => TsNoMatch[A](input)
      }
    }

  def ~>[B <: HList](next: Pattern[B]): Pattern[B] =
    Pattern { input =>
      self.apply(input) match {
        case TsMatch(_, rest) => next(rest)
        case _                => TsNoMatch[B](input)
      }
    }

  def |[B <: HList](right: Pattern[B]): Pattern[Either[A, B] :: HNil] = alt(self)(right)

}
