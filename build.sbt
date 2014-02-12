name := "Sat4jExperiments"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5" withSources() withJavadoc()

//libraryDependencies += "org.specs2" % "specs2_2.10" % "2.3.6" withJavadoc()

//libraryDependencies += "com.jsuereth" % "scala-arm_2.10" % "1.3" withJavadoc()

//scalacOptions in Test ++= Seq("-Yrangepos")

//resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
