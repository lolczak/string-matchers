package io.rebelapps.text.typesafe

import shapeless.HList

sealed trait TsMatcherResult[A <: HList] {

  val next: List[Char]

  def mapMatches[B <: HList](f: A => B): TsMatcherResult[B]

}

case class TsMatch[A <: HList](matches: A, override val next: List[Char]) extends TsMatcherResult[A] {
  override def mapMatches[B <: HList](f: A => B): TsMatcherResult[B] = TsMatch(f(matches), next)
}

case class TsNoMatch[A <: HList](override val next: List[Char]) extends TsMatcherResult[A] {
  override def mapMatches[B <: HList](f: A => B): TsMatcherResult[B] = TsNoMatch[B](next)
}