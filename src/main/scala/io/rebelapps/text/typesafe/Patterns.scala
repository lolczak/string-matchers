package io.rebelapps.text.typesafe

import cats.Monoid
import shapeless.{::, HList, HNil}

import scala.annotation.tailrec

object Patterns {

  lazy val acceptChar = (f: Char => Boolean) =>
    Pattern[String :: HNil] {
      case Nil                    => TsNoMatch[String :: HNil](Nil)
      case input if f(input.head) => TsMatch[String :: HNil](input.head.toString :: HNil, input.tail)
      case input                  => TsNoMatch[String :: HNil](input)
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
        TsMatch[String :: HNil](const :: HNil, input.drop(const.length))
      } else {
        TsNoMatch[String :: HNil](input)
      }
    }

  private lazy val punctuationCharacters = """][!"#$%&'()*+,./:;<=>?@\^_`{|}~-""".toCharArray.toSet

  lazy val punct = acceptChar(punctuationCharacters.contains)

  def range(min: Char, max: Char) = acceptChar(ch => ch >= min && ch <= max)

  lazy val eos: Pattern[HNil] =
    Pattern { input =>
      if (input.isEmpty) TsMatch[HNil](HNil, input)
      else TsNoMatch[HNil](input)
    }

  def rep1[A <: HList](p: Pattern[A]):Pattern[List[A] :: HNil] =
    Pattern { next =>
      @tailrec
      def loop(next: List[Char], acc: List[A] = List.empty): TsMatcherResult[List[A] :: HNil] = {
        p(next) match {
          case TsNoMatch(_) if acc.isEmpty  => TsNoMatch[List[A] :: HNil](next)
          case TsNoMatch(_) if acc.nonEmpty => TsMatch[List[A] :: HNil](acc :: HNil, next)
          case TsMatch(t, n)                => loop(n, acc :+ t)
        }
      }

      loop(next)
    }

  def rep0[A <: HList](p: Pattern[A]): Pattern[List[A] :: HNil] =
    Pattern { next =>
      @tailrec
      def loop(next: List[Char], acc: List[A] = List.empty): TsMatcherResult[List[A] :: HNil] = {
        p(next) match {
          case TsNoMatch(_)  => TsMatch[List[A] :: HNil](acc :: HNil, next)
          case TsMatch(t, n) => loop(n, acc :+ t)
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
        case TsNoMatch(_)  => TsMatch[HNil](HNil, input)
        case TsMatch(_, _) => TsNoMatch[HNil](input)
      }
    }

  def guard[A <: HList](p: Pattern[A]): Pattern[HNil] =
    Pattern { input =>
      p(input) match {
        case TsNoMatch(_)  => TsNoMatch[HNil](input)
        case TsMatch(_, _) => TsMatch[HNil](HNil, input)
      }
    }

  def opt[A <: HList](p: Pattern[A]): Pattern[Option[A] :: HNil] =
    Pattern { input =>
      p(input) match {
        case TsNoMatch(_)  => TsMatch[Option[A] :: HNil](None :: HNil, input)
        case TsMatch(t, n) => TsMatch[Option[A] :: HNil](Some(t) :: HNil, n)
      }
    }

  def alt[A <: HList, B <: HList](left: => Pattern[A])(right: => Pattern[B]): Pattern[Either[A, B] :: HNil] =
    Pattern { input =>
      left(input) match {
        case TsMatch(matches, next) => TsMatch[Either[A, B] :: HNil](Left(matches) :: HNil, next)
        case TsNoMatch(_)           => right(input).mapMatches(m => Right(m) :: HNil)
      }
    }

  def <+>[A](p: Pattern[List[A :: HNil] :: HNil])(implicit M: Monoid[A]): Pattern[A :: HNil] = con(p)

  def con[A](p: Pattern[List[A :: HNil] :: HNil])(implicit M: Monoid[A]): Pattern[A :: HNil] =
    Pattern { input =>
      p(input) match {
        case TsMatch(matches :: HNil, next) =>
          val concatenated = matches.foldLeft(M.empty) { case (acc, elem :: HNil) => M.combine(acc, elem) }
          TsMatch[A :: HNil](concatenated :: HNil, next)

        case TsNoMatch(next)        => TsNoMatch[A :: HNil](next)
      }
    }

}
