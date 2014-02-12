name := "Sat4jExperiments"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5" withSources() withJavadoc()

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test" withSources() withJavadoc()

//libraryDependencies += "org.specs2" %% "specs2" % "2.3.7" % "test" withSources() withJavadoc()

//libraryDependencies += "com.jsuereth" % "scala-arm_2.10" % "1.3" withJavadoc()

scalacOptions in Test ++= Seq("-Yrangepos")

//resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

parallelExecution in Test := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
