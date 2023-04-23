ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

// Add dependency for json-handling package
libraryDependencies += "org.json4s" %% "json4s-jackson" % "4.0.3"

// Add dependency for unittest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

// Add dependency for scala.swing
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "project-triangle-puzzle"
  )
// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" % "scalafx_3" % "19.0.0-R30"
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "14.0.1" classifier osName
)