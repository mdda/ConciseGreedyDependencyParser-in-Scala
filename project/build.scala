import sbt._
import Keys._

object ConciseGreedyDependencyParserBuild extends Build {
  def scalaSettings = Seq(
    scalaVersion := "2.10.3",  // 2.10.0 was too low to get implicit macros (needed for pickling)
    scalacOptions ++= Seq(
      "-optimize",
      "-unchecked",
      "-deprecation"
    )
  )
  
  def librarySettings = Seq(
    libraryDependencies ++= Seq(
      //"com.mycompany" % "mylibrary" % "0.1-SNAPSHOT"
      
      //"org.scala-lang" %% "scala-pickling" % "0.8.0"  
      //"org.scala-lang" %% "scala-pickling" % "0.9.0-SNAPSHOT"  

      //"com.typesafe.akka" %% "akka-actor" % "2.2.3",
      "org.zeromq" % "jeromq" % "0.3.3"
      //"org.zeromq" % "zeromq-scala-binding" % "0.0.9"
    ),
    resolvers ++= Seq(
      //Resolver.sonatypeRepo("snapshots") // Needed to use the scala-pickling 0.9.0-SNAPSHOT
      Resolver.sonatypeRepo("releases")    // Needed to get the zeromq-scala-binding
    )
  )
  
  def buildSettings =
    Project.defaultSettings ++
    scalaSettings ++ 
    librarySettings
    
  lazy val root = {
    val settings = buildSettings ++ Seq( name := "ConciseGreedyDependencyParser" )
    Project(id = "ConciseGreedyDependencyParser", base = file("."), settings = settings)
  }
  
}
