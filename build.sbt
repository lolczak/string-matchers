name := "string-matchers"
organization := "io.rebelapps"
crossScalaVersions := Seq("2.11.12", "2.12.8")

val catsVersion = "1.4.0"
val shapelessVersion = "2.3.3"
val scalaTestVersion = "3.0.5"
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
    "-deprecation",
    "-Ypartial-unification",
    "-encoding", "utf8",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-value-discard"
)

