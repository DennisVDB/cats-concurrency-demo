name := "cats-concurrency-demo"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.2"

libraryDependencies ++=
  Seq(
    "org.typelevel"              %% "cats-core"      % "2.0.0",
    "org.typelevel"              %% "cats-effect"    % "2.1.3",
    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2",
    "ch.qos.logback"             % "logback-classic" % "1.2.3"
  )

// scalac options come from the sbt-tpolecat plugin so need to set any here

addCompilerPlugin(
  "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
)

scalacOptions -= "-Xfatal-warnings"
