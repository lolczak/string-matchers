package io.rebelapps.text.typesafe

import shapeless.HList
import shapeless.ops.hlist.Prepend

class PatternOps[A <: HList](self: Pattern[A]) {

  def ~[B <: HList](next: Pattern[B])(implicit prepend: Prepend[A, B]): Pattern[prepend.Out] =
    Pattern { input =>
      self.apply(input) match {
        case TsMatch(matches, rest) => next(rest).mapMatches[prepend.Out](suffix => matches.++(suffix)(prepend))
        case _                      => TsNoMatch[prepend.Out](input)
      }
    }

}
