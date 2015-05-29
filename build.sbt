
name := """play-scala"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.scalikejdbc" %% "scalikejdbc"       			% "2.2.6",
  "org.scalikejdbc" %% "scalikejdbc-config"			% "2.2.6",
  "org.scalikejdbc" %% "scalikejdbc-play-plugin"	% "2.3.6",
  "org.scalikejdbc" %% "scalikejdbc-play-dbplugin-adapter" % "2.3.6",
  "com.github.nscala-time" %% "nscala-time" % "2.0.0",
  "io.github.cloudify" %% "spdf" % "1.3.1"
)

PlayKeys.playWatchService := play.sbtplugin.run.PlayWatchService.sbt(pollInterval.value)

scalikejdbcSettings

scalacOptions += "-feature"

fork in run := false
