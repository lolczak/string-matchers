package io.rebelapps.text.typesafe

import cats.Monoid
import shapeless.{::, HList, HNil}

import scala.annotation.tailrec

object Patterns {

  lazy val acceptChar = (f: Char => Boolean) =>
    Pattern[String :: HNil] {
      case Nil                    => NoMatch[String :: HNil](Nil)
      case input if f(input.head) => Match[String :: HNil](input.head.toString :: HNil, input.tail)
      case input                  => NoMatch[String :: HNil](input)
    }

  lazy val ch = (ch: Char) => acceptChar(_ == ch)

  lazy val space = acceptChar(_.isWhitespace)

  lazy val s = space

  lazy val digit = acceptChar(_.isDigit)

  lazy val d = digit

  lazy val word = acceptChar(ch => ch.isLetterOrDigit || ch == '_')

  lazy val w = word

  lazy val alpha = acceptChar(_.isLetter)

  lazy val a = alpha

  lazy val txt = (const: String) =>
    Pattern { input =>
      if (input.startsWith(const.toSeq)) {
        Match[String :: HNil](const :: HNil, input.drop(const.length))
      } else {
        NoMatch[String :: HNil](input)
      }
    }

  private lazy val punctuationCharacters = """][!"#$%&'()*+,./:;<=>?@\^_`{|}~-""".toCharArray.toSet

  lazy val punct = acceptChar(punctuationCharacters.contains)

  def range(min: Char, max: Char) = acceptChar(ch => ch >= min && ch <= max)

  lazy val eos: Pattern[HNil] =
    Pattern { input =>
      if (input.isEmpty) Match[HNil](HNil, input)
      else NoMatch[HNil](input)
    }

  def rep1[A <: HList](p: Pattern[A]):Pattern[List[A] :: HNil] =
    Pattern { next =>
      @tailrec
      def loop(next: List[Char], acc: List[A] = List.empty): MatcherResult[List[A] :: HNil] = {
        p(next) match {
          case NoMatch(_) if acc.isEmpty  => NoMatch[List[A] :: HNil](next)
          case NoMatch(_) if acc.nonEmpty => Match[List[A] :: HNil](acc :: HNil, next)
          case Match(t, n)                => loop(n, acc :+ t)
        }
      }

      loop(next)
    }

  def rep0[A <: HList](p: Pattern[A]): Pattern[List[A] :: HNil] =
    Pattern { next =>
      @tailrec
      def loop(next: List[Char], acc: List[A] = List.empty): MatcherResult[List[A] :: HNil] = {
        p(next) match {
          case NoMatch(_)  => Match[List[A] :: HNil](acc :: HNil, next)
          case Match(t, n) => loop(n, acc :+ t)
        }
      }

      loop(next)
    }

  //    end ^^^ List.empty | (p ~ repTill(p, end)) ^^ { case x ~ xs => x :: xs }
  def repTill[A <: HList, B <: HList](p: Pattern[A], q: Pattern[B]): Pattern[List[A] :: HNil]  =  {
    val end = guard(q) <> (_ => List.empty[A] :: HNil)
    val head = p <> (hlist => List(hlist) :: HNil)
    lazy val rec = (head ~ repTill(p, q)) <> { case h :: t :: HNil => (h ++ t) :: HNil}

    (end | rec) <> {
      case Left(r) :: HNil  => r
      case Right(r) :: HNil => r
    }
  }

  def not[A <: HList](p: Pattern[A]): Pattern[HNil] =
    Pattern { input =>
      p(input) match {
        case NoMatch(_)  => Match[HNil](HNil, input)
        case Match(_, _) => NoMatch[HNil](input)
      }
    }

  def guard[A <: HList](p: Pattern[A]): Pattern[HNil] =
    Pattern { input =>
      p(input) match {
        case NoMatch(_)  => NoMatch[HNil](input)
        case Match(_, _) => Match[HNil](HNil, input)
      }
    }

  def opt[A <: HList](p: Pattern[A]): Pattern[Option[A] :: HNil] =
    Pattern { input =>
      p(input) match {
        case NoMatch(_)  => Match[Option[A] :: HNil](None :: HNil, input)
        case Match(t, n) => Match[Option[A] :: HNil](Some(t) :: HNil, n)
      }
    }

  def alt[A <: HList, B <: HList](left: => Pattern[A])(right: => Pattern[B]): Pattern[Either[A, B] :: HNil] =
    Pattern { input =>
      left(input) match {
        case Match(matches, next) => Match[Either[A, B] :: HNil](Left(matches) :: HNil, next)
        case NoMatch(_)           => right(input).mapMatches(m => Right(m) :: HNil)
      }
    }

  def <+>[A](p: Pattern[List[A :: HNil] :: HNil])(implicit M: Monoid[A]): Pattern[A :: HNil] = con(p)

  def con[A](p: Pattern[List[A :: HNil] :: HNil])(implicit M: Monoid[A]): Pattern[A :: HNil] =
    Pattern { input =>
      p(input) match {
        case Match(matches :: HNil, next) =>
          val concatenated = matches.foldLeft(M.empty) { case (acc, elem :: HNil) => M.combine(acc, elem) }
          Match[A :: HNil](concatenated :: HNil, next)

        case NoMatch(next)        => NoMatch[A :: HNil](next)
      }
    }

}
