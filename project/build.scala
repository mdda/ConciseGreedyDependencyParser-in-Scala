import sbt._
import Keys._

object ConciseGreedyDependencyParserBuild extends Build {
  def scalaSettings = Seq(
    scalaVersion := "2.10.3",  // 2.10.0 was too low to get implicit macros (needed for pickling)
    scalacOptions ++= Seq(
      "-optimize",
      "-unchecked",
      "-deprecation"
    ),
    // This allows one to      :: "kill -9 `ps -h | grep java | grep -v sbt-launch | grep -v grep | awk '{print $1}'`"
    //   or, more specifically :: "kill -9 `ps -h | grep ConciseGreedyDependencyParser.Main | grep -v grep | awk '{print $1}'`"
    fork in run := true
  )
  
  def librarySettings = Seq(
    libraryDependencies ++= Seq(
      //"com.mycompany" % "mylibrary" % "0.1-SNAPSHOT"
      
      //"org.scala-lang" %% "scala-pickling" % "0.8.0"  
      //"org.scala-lang" %% "scala-pickling" % "0.9.0-SNAPSHOT"  

      //"com.typesafe.akka" %% "akka-actor" % "2.2.3",
      "org.zeromq" % "jeromq" % "0.3.3"
      
      //"org.zeromq" % "zeromq-scala-binding" % "0.0.9" // Not present in releases, apparently
      //"org.zeromq" %% "zeromq-scala-binding" % "0.0.6" // Has 'issues'

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
