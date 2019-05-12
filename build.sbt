name := "string-matchers"
organization := "io.rebelapps"
crossScalaVersions := Seq("2.11.12", "2.12.8")

val catsVersion = "1.4.0"
val catsEffectVersion = "1.0.0"
val circeVersion = "0.10.1"
val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.14.0"
val logbackVersion = "1.2.3"
val slf4jVersion = "1.7.25"
val commonsLangVersion = "3.3.2"
val scalaLoggingVersion = "3.7.2"

lazy val functional = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-effect" % catsEffectVersion,
)
lazy val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
)
lazy val testLibs = Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % Test
)

libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-lang3" % commonsLangVersion,
    "org.slf4j" % "slf4j-api" % slf4jVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion,
    "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion exclude("org.scala-lang", "scala-reflect")
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

