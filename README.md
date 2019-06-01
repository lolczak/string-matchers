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

```
scala> import cats.implicits._, io.rebelapps.text.Patterns._,  scala.language.reflectiveCalls, shapeless._
import cats.implicits._
import io.rebelapps.text.Patterns._
import scala.language.reflectiveCalls
import shapeless._

scala> val servicePattern = con(rep1(acceptChar(!_.isWhitespace))) <~ ws
servicePattern: io.rebelapps.text.Pattern[String :: shapeless.HNil] = <function1>

scala> val portWithProtocol = con(d.+) ~ (txt("/tcp") or txt("/udp")) <> { case port :: proto :: HNil => (port + proto) :: HNil }
portWithProtocol: io.rebelapps.text.Pattern[String :: shapeless.HNil] = <function1>

scala> val portPattern = ws ~> (portWithProtocol or con(d.+)) <~ ws
portPattern: io.rebelapps.text.Pattern[String :: shapeless.HNil] = <function1>

scala> val descriptionPattern = con(any.+) ^^ ((_: String).trim)
descriptionPattern: io.rebelapps.text.Pattern[String :: shapeless.HNil] = <function1>

scala> val PortPattern = (servicePattern ~ portPattern ~ descriptionPattern).tupled.matcher
PortPattern: io.rebelapps.text.MatcherExtractor[(String, String, String, Seq[Nothing])] = io.rebelapps.text.MatcherExtractor@41c380e1

scala> "ssh              22/tcp    The Secure Shell (SSH) Protocol" match {
     |   case PortPattern(service, port, description) =>
     |     println("Service is: " + service)
     |     println("Port is: " + port)
     |     println("Description: " + description)
     | }
Service is: ssh
Port is: 22/tcp
Description: The Secure Shell (SSH) Protocol

```
