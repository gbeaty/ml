lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.13.12"
    )),
    name := "ml"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
