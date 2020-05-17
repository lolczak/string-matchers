name := "string-matchers"
organization := "io.rebelapps"
crossScalaVersions := Seq("2.11.12", "2.12.8", "2.13.2")

val catsVersion = "2.0.0"
val shapelessVersion = "2.3.3"
val scalaTestVersion = "3.1.0"
val scalaCheckVersion = "1.14.0"

lazy val testLibs = Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % Test
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion % Provided,
  "com.chuusai" %% "shapeless" % shapelessVersion
) ++ testLibs

lazy val `string-matchers` = project in file(".")

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-encoding",
  "utf8",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  "-Ywarn-value-discard"
)
