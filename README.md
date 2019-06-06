# string-matchers

**1. About**

*string-matchers* - is a modular, reusable and type safe regex-like matcher that you can pattern match in Scala.

**2. Examples**

*a) date matcher*

```
scala> import cats.implicits._, io.rebelapps.text.Patterns._,  scala.language.reflectiveCalls

scala> val Pattern = (num <~ ch('-')) ~ (num <~ ch('-')) ~ num
Pattern: io.rebelapps.text.Pattern[Int :: Int :: Int :: shapeless.HNil] = <function1>

scala> "2019-09-10" match { case Pattern(year ::  month ::  day :: HNil) => println(s"Year:$year month:$month day:$day") }
Year:2019 month:9 day:10

```

*b) email matcher*

```
scala> import cats.implicits._, io.rebelapps.text.Patterns._,  scala.language.reflectiveCalls, shapeless._

scala> val Pattern = (w.+.<+> ~ ch('@') ~ any.+.<+>).tupled.matcher
Pattern: io.rebelapps.text.MatcherExtractor[(String, String, String, Seq[Nothing])] = io.rebelapps.text.MatcherExtractor@1976b6b6

scala> "hello@example.com" match {
     |   case Pattern(username, _, domain) => println(s"Username: $username, domain: $domain")
     | }
Username: hello, domain: example.com


```
