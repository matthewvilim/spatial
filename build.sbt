val spatial_version   = "1.0-SNAPSHOT"
val scala_version     = "2.12.6"
val paradise_version  = "2.1.0"
val scalatestVersion  = "3.0.5"

name := "spatial"
organization := "edu.stanford.ppl"
trapExit := false
isSnapshot := true

val common = Seq(
  organization := "edu.stanford.cs.dawn",
  scalaVersion := scala_version,
  version := spatial_version,

  /** External Libraries (e.g. maven dependencies) **/
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scala_version,  // Reflection
    "org.scalatest" %% "scalatest" % scalatestVersion,	 // Testing
    "com.github.scopt" %% "scopt" % "3.7.0",             // Command line args
    "org.scala-lang.modules" %% "scala-xml" % "1.1.0",
    "com.github.pureconfig" %% "pureconfig" % "0.9.2",

    // These are a bit bulky, leaving them out in favor of a stripped down version for now
    //"org.apache.commons" % "commons-lang3" % "3.3.2",
    //"commons-io" % "commons-io" % "2.5"
  ),

  /** Scalac Options **/
  scalacOptions += "-target:jvm-1.8",               // JVM 1.8
  scalacOptions ++= Seq("-encoding", "UTF-8"),      // Encoding using UTF-8
  scalacOptions += "-unchecked",                    // Enable additional warnings
  scalacOptions += "-deprecation",                  // Enable warnings on deprecated usage
  scalacOptions += "-feature",                      // Warnings for features requiring explicit import
  scalacOptions += "-Xfatal-warnings",              // Warnings are errors
  scalacOptions += "-language:higherKinds",         // Globally enable higher kinded type parameters
  scalacOptions += "-language:implicitConversions", // Globally enable implicit conversions
  scalacOptions += "-language:experimental.macros", // Globally enable macros
  scalacOptions += "-language:existentials",        // Globally enable existentials
  scalacOptions += "-Yno-generic-signatures",       // Suppress generation of generic signatures in bytecode
  scalacOptions += "-Xfuture",                      // Enable "future language features"
  scalacOptions += "-opt:l:method,inline",          // Enable method optimizations, inlining
  scalacOptions += "-opt-warnings:none",            // Disable optimization warnings
  scalacOptions in (Compile, doc) += "-diagrams",   // Generate type hiearchy graph in scala doc

  /** Project Structure **/
  resourceDirectory in Compile := baseDirectory(_/ "resources").value,
  scalaSource in Compile := baseDirectory(_/"src").value,
  //scalaSource in Test := baseDirectory(_/"test").value,

  /** Testing **/
  scalacOptions in Test ++= Seq("-Yrangepos"),

  /** Macro Paradise **/
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % paradise_version cross CrossVersion.full),

  /** Release **/
  publishArtifact := true,
  isSnapshot := true,

  homepage := Some(url("https://spatial.stanford.edu")),
  scmInfo := Some(ScmInfo(url("https://github.com/stanford-ppl/spatial"),
                              "git@github.com:stanford-ppl/spatial.git")),
  developers := List(Developer("mattfel1",
                               "Matthew Feldman",
                               "mattfel@stanford.edu",
                               url("https://github.com/mattfel1")),
                     Developer("dkoeplin",
                               "David Koeplinger",
                               "dkoeplin@stanford.edu",
                               url("https://github.com/dkoeplin")),
                     Developer("raghup17",
                               "Raghu Prabhakar",
                               "raghup17@stanford.edu",
                               url("https://github.com/raghup17")),
                     Developer("yaqiz",
                               "Yaqi Zhang",
                               "yaqiz@stanford.edu",
                               url("https://github.comyaqiz/"))
                    ),
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),

  publishMavenStyle := true
)


/** Projects **/
lazy val utils  = project.settings(common)
lazy val emul   = project.settings(common)
lazy val fringe = project.settings(common).settings(scalaVersion := "2.11")
lazy val models = project.settings(common)
lazy val forge  = project.settings(common).dependsOn(utils)
lazy val poly   = project.settings(common).dependsOn(utils)
lazy val argon  = project.settings(common).dependsOn(utils, forge, emul)

lazy val spatial = (project in file(".")).settings(
  common ++ Seq(scalaSource in Test := baseDirectory(_/"test").value)
).dependsOn(forge, emul, argon, models, poly)
lazy val apps = project.settings(common).dependsOn(spatial)

/** Testing Projects **/
lazy val appsTest = project.settings(
  common ++ Seq(scalaSource in Test := baseDirectory.in(spatial).value/"test/spatial/tests/apps/"),
).dependsOn(spatial)
lazy val compilerTest = project.settings(
  common ++ Seq(scalaSource in Test := baseDirectory.in(spatial).value/"test/spatial/tests/compiler/"),
).dependsOn(spatial)
lazy val RosettaTest = project.settings(
  common ++ Seq(scalaSource in Test := baseDirectory.in(spatial).value/"test/spatial/tests/Rosetta/"),
).dependsOn(spatial)
lazy val syntaxTest = project.settings(
  common ++ Seq(scalaSource in Test := baseDirectory.in(spatial).value/"test/spatial/tests/syntax/"),
).dependsOn(spatial)
lazy val denseTest = project.settings(
  common ++ Seq(scalaSource in Test := baseDirectory.in(spatial).value/"test/spatial/tests/feature/dense/"),
).dependsOn(spatial)
lazy val featureTest = project.settings(
  common ++ Seq(scalaSource in Test := baseDirectory.in(spatial).value/"test/spatial/tests/feature/"),
).aggregate(denseTest).dependsOn(spatial)

lazy val tests = project.settings(common).aggregate(appsTest, compilerTest, RosettaTest, syntaxTest, featureTest)

/** Set number of threads for testing **/
val threadsOrDefault: Int = Option(System.getProperty("maxthreads")).getOrElse("1").toInt
Global / concurrentRestrictions += Tags.limit(Tags.Test, threadsOrDefault)

addCommandAlias("make", "; project denseTest; test:compile")
