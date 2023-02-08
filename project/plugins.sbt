
// sbt-sonatype plugin used to publish artifact to maven central via sonatype nexus
// sbt-pgp plugin used to sign the artifcat with pgp keys
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.17")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")

// add `publish / skip := true` at top of build.sbt
// add sonatype credentials to build.sbt
// sbt publishSigned
// sbt sonatypeBundleRelease
//   <- will fail: Supplied file ... is a not an existing directory!
//   -> Create directory by hand `mkdir -p ...` and rerun
