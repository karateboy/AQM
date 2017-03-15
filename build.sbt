
name := """aqms"""

version := "1.1.3"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalikejdbc" %% "scalikejdbc"                  % "2.4.1",
  "org.scalikejdbc" %% "scalikejdbc-config"           % "2.4.1",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.5.1",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "io.github.cloudify" %% "spdf" % "1.3.1",
  "com.typesafe.play" %% "play-mailer" % "4.0.0"
)
mappings in Universal ++=
  (baseDirectory.value / "report_template" * "*" get) map
    (x => x -> ("report_template/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "notification" * "*" get) map
    (x => x -> ("notification/" + x.getName))
    
PlayKeys.fileWatchService := play.runsupport.FileWatchService.sbt(2000)

scalacOptions += "-feature"

fork in run := false
