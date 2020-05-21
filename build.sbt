name := "truediff"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-Ymacro-annotations"
//, "-Ymacro-debug-lite"
  , "-J-Xss10m"
)

// https://mvnrepository.com/artifact/org.apache.commons/commons-collections4
libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.3" % "test"

