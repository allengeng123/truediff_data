name := "truechange-truediff"


ThisBuild / organization := "de.uni-mainz.informatik.pl"
ThisBuild / version := "0.1.1"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / homepage := Some(url("https://gitlab.rlp.net/plmz/truediff"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://gitlab.rlp.net/plmz/truediff"), "git@gitlab.rlp.net:plmz/truediff.git"))
ThisBuild / developers := List(Developer("PLMZ", "Programming Languages research group of the Johannes Gutenberg University Mainz", "", url("https://www.pl.informatik.uni-mainz.de/")))
ThisBuild / licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := sonatypePublishToBundle.value
Global / useGpgPinentry := true

ThisBuild / scalacOptions += "-target:11"

lazy val root = (project in file("."))
  .aggregate(truechange, truediff)
  .settings(skip in publish := true)

lazy val truechange = project.settings(
  name := "truechange",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
)

lazy val truediff = project.dependsOn(truechange).settings(
  name := "truediff",
  scalacOptions ++= Seq(
    "-Ymacro-annotations"
    , "-J-Xss10m"
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.apache.commons" % "commons-collections4" % "4.4",

    "org.scalactic" %% "scalactic" % "3.1.0" % "test",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    "com.lihaoyi" %% "fastparse" % "2.1.3" % "test"
  )
)






