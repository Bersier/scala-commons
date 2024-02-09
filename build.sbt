ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "scala-commons",
    scalacOptions ++= Seq(
      "-Wnonunit-statement",
      "-Ycheck-all-patmat",
      "-Ycheck-reentrant",
      "-Ycook-comments",
      "-Ydebug-pos",
      "-Yexplicit-nulls",
      "-Ykind-projector:underscores",
      "-Yrequire-targetName",
      "-Ysafe-init",
      "-deprecation",
      "-explain",
      "-feature",
      "-language:strictEquality",
      "-new-syntax",
      "-release:21",
      "-source:future",
      "-unchecked",
    )
  )
