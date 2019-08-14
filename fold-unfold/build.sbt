enablePlugins(TutPlugin)
watchSources ++= tutSourceDirectory.map({ path => (path ** "*.html").get }).value

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0"
)

scalaVersion := "2.12.8"
