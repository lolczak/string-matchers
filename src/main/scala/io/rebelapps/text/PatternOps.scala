package io.rebelapps.text

import cats.Monoid
import io.rebelapps.text.Patterns._
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList, HNil}

class PatternOps[A <: HList](self: Pattern[A]) {

  def ~[B <: HList](next: Pattern[B])(implicit prepend: Prepend[A, B]): Pattern[prepend.Out] =
    Pattern { input =>
      self.apply(input) match {
        case Match(matches, rest) => next(rest).mapMatches[prepend.Out](suffix => matches.++(suffix)(prepend))
        case _                      => NoMatch[prepend.Out](input)
      }
    }


  def <~[B <: HList](next: Pattern[B]): Pattern[A] =
    Pattern { input =>
      self.apply(input) match {
        case Match(matches, rest) => next(rest) mapMatches[A] (_ => matches)
        case _                      => NoMatch[A](input)
      }
    }

  def ~>[B <: HList](next: Pattern[B]): Pattern[B] =
    Pattern { input =>
      self.apply(input) match {
        case Match(_, rest) => next(rest)
        case _                => NoMatch[B](input)
      }
    }

  def |[B <: HList](right: => Pattern[B]): Pattern[Either[A, B] :: HNil] = alt(self)(right)

  def ||(right: => Pattern[A]): Pattern[A :: HNil] = altConformant(self)(right)

  def or[B](right: => Pattern[B :: HNil])(implicit ev: A <:< (B :: HNil)): Pattern[B :: HNil] =
    altConformantSingle[B](self.asInstanceOf[Pattern[B :: HNil]])(right)

  def <+>[B](implicit ev: A <:< (List[B :: HNil] :: HNil), M: Monoid[B]): Pattern[B :: HNil] =
    con[B](self.asInstanceOf[Pattern[List[B :: HNil] :: HNil]])

}
