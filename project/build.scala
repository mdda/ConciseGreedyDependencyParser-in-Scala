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
  
  def buildSettings =
    Project.defaultSettings ++
    scalaSettings

  lazy val root = {
    val settings = buildSettings ++ Seq(name := "ConciseGreedyDependencyParser")
    Project(id = "ConciseGreedyDependencyParser", base = file("."), settings = settings)
  }
  
}
