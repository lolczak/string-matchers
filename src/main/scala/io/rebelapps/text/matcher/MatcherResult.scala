package io.rebelapps.text.matcher

sealed trait MatcherResult {

  val next: List[Char]

  def mapMatches(f: List[String] => List[String]): MatcherResult

  def mapMatchesPartial(f: PartialFunction[List[String], List[String]]): MatcherResult

}

case class Match(matches: List[String], override val next: List[Char]) extends MatcherResult {

  override def mapMatches(f: List[String] => List[String]): MatcherResult = this.copy(matches = f(matches))

  override def mapMatchesPartial(f: PartialFunction[List[String], List[String]]): MatcherResult =
    if (f isDefinedAt matches) this.copy(matches = f(matches))
    else this

}

object Match {

  def apply(term: String, next: List[Char]): Match = Match(List(term), next)

}

case class NoMatch(override val next: List[Char]) extends MatcherResult {


  override def mapMatches(f: List[String] => List[String]): MatcherResult = this

  override def mapMatchesPartial(f: PartialFunction[List[String], List[String]]): MatcherResult = this

}
