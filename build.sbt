ThisBuild / organization := "de.uni-mainz.informatik.pl"

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.1"


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






