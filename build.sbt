name := "scifn"

homepage := Some(url("https://github.com/scifn/scifn"))

licenses := Seq("MIT License" -> url("http://opensource.org/licenses/MIT"))

description := """library for easily turning expressions into functions"""

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
    <url>https://scifn.github.io</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:scifn/scifn</url>
      <connection>scm:git:git@github.com:scifn/scifn.git</connection>
    </scm>
    <developers>
      <developer>
        <id>deaktator</id>
        <name>R M Deak</name>
        <url>https://deaktator.github.io</url>
      </developer>
    </developers>
)

lazy val commonSettings = Seq(
  organization := "com.github.scifn",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.6",
  crossScalaVersions := Seq("2.11.6", "2.10.5"),
  crossPaths := true,
  incOptions := incOptions.value.withNameHashing(true),
  javacOptions ++= Seq("-Xlint:unchecked"),
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-Yinline",
    "-Yinline-warnings",
    "-Yclosure-elim",
    "-Ydead-code",
    "-Xverify",
    "-Ywarn-inaccessible",
    "-Ywarn-dead-code"
  ),

  scalacOptions <++= scalaVersion map {
    case v: String if v.split("\\.")(1).toInt >= 11 =>
      Seq(
        "-Ywarn-unused",
        "-Ywarn-unused-import",

        // These options don't play nice with IntelliJ.  Comment them out to debug.
        "-Ybackend:GenBCode",
        "-Ydelambdafy:method",
        "-Yopt:l:project",
        "-Yconst-opt"
      )
    case _ =>
      Seq()
  }
)

// ====================   Disable packaging root project   ====================
//  Paul P: http://stackoverflow.com/a/25653777
Keys.`package` :=  file("")

packageBin in Global :=  file("")

packagedArtifacts :=  Map()
// ====================   Disable packaging root project   ====================

lazy val root = project.in( file(".") ).
  // To run benchmarks with tests, add 'bench' to the aggregate list
  aggregate(gen).
  settings(commonSettings: _*).
  settings ()

lazy val gen = project.in( file("scifn-gen") ).
  settings(commonSettings: _*).
  settings (
    name := "scifn-gen",

    // Because 2.10 runtime reflection is not thread-safe, tests fail non-deterministically.
    // This is a hack to make tests pass by not allowing the tests to run in parallel.
    parallelExecution in Test := false,

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.slf4j" % "slf4j-api" % "1.7.10",
      "org.scalatest" %% "scalatest" % "2.2.5" % "test",
      "org.slf4j" % "slf4j-log4j12" % "1.7.10" % "test"
    )
  ).
  settings(
    // Allow macros and quasiquotes on 2.10.x.
    // http://docs.scala-lang.org/overviews/quasiquotes/setup.html
    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value
        // in Scala 2.10, quasiquotes are provided by macro paradise
        case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary
          )
      }
    }
  )

lazy val bench = project.in( file("scifn-bench") ).
  dependsOn(gen).
  settings(commonSettings: _*).
  settings (
    name := "scifn-bench",
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.6" % "test"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    parallelExecution in Test := false
  )

// Site plugin
site.settings

site.includeScaladoc()

site.jekyllSupport()


//com.typesafe.sbt.site.JekyllSupport.requiredGems := Map(
//  "jekyll" -> "0.11.2",
//  "liquid" -> "2.3.0"
//)
