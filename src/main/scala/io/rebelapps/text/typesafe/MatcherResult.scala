package io.rebelapps.text.typesafe

import shapeless.HList

sealed trait MatcherResult[A <: HList] {

  val next: List[Char]

  def mapMatches[B <: HList](f: A => B): MatcherResult[B]

}

case class Match[A <: HList](matches: A, override val next: List[Char]) extends MatcherResult[A] {

  override def mapMatches[B <: HList](f: A => B): MatcherResult[B] = Match(f(matches), next)

}

case class NoMatch[A <: HList](override val next: List[Char]) extends MatcherResult[A] {

  override def mapMatches[B <: HList](f: A => B): MatcherResult[B] = NoMatch[B](next)

}