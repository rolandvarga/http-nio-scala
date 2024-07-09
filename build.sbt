scalaVersion := "2.13.12"

name := "impl-http-scala-dev"
organization := "io.rolandvarga"
version := "1.0"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.32.0",
  "org.scalatest" %% "scalatest" % "3.2.18" % "test",
  "org.scalatestplus" %% "mockito-5-10" % "3.2.18.0" % "test",
//  "org.scalatestplus" %% "mockito-5-10" % "3.2.18.0" % Test,
)

