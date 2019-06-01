# string-matchers

**1. About**

*string-matchers* - is a modular, reusable and type safe regex-like matcher that you can pattern match in Scala.

**2. Examples**

```
scala> import cats.implicits._, io.rebelapps.text.Patterns._,  scala.language.reflectiveCalls
import cats.implicits._
import io.rebelapps.text.Patterns._
import scala.language.reflectiveCalls

scala> val Pattern = ((num <~ ch('-')) ~ (num <~ ch('-')) ~ num).tupled.matcher
Pattern: io.rebelapps.text.MatcherExtractor[(Int, Int, Int, Seq[Nothing])] = io.rebelapps.text.MatcherExtractor@5b752914

scala> "2019-09-10" match {
     |   case Pattern(year, month, day) => println(s"Year:$year month:$month day:$day")
     | }
Year:2019 month:9 day:10

```
