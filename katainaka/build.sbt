name := "katainaka"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
  "org.mockito" % "mockito-core" % "2.10.0" % Test,
  "eu.timepit" %% "refined" % "0.8.7",
  "eu.timepit" %% "refined-scalacheck" % "0.8.7" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
  // 警告をエラーにする（お好みに応じて）
  , "-Xfatal-warnings"
)
