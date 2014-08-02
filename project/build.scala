import sbt._
import Keys._

object ConciseGreedyDependencyParserBuild extends Build {
  def scalaSettings = Seq(
    scalaVersion := "2.10.0",
    scalacOptions ++= Seq(
      "-optimize",
      "-unchecked",
      "-deprecation"
    )
  )
  
  def librarySettings = Seq(
    libraryDependencies ++= Seq(
      //"com.mycompany" % "mylibrary" % "0.1-SNAPSHOT"
      "org.scala-lang" %% "scala-pickling" % "0.8.0"  
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
