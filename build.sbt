
name := """aqms"""

version := "1.1.28"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalikejdbc" %% "scalikejdbc"                  % "2.5.2",
  "org.scalikejdbc" %% "scalikejdbc-config"           % "2.5.2",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.5.1",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "io.github.cloudify" %% "spdf" % "1.3.1",
  "com.typesafe.play" %% "play-mailer" % "4.0.0"
)
// https://mvnrepository.com/artifact/org.apache.poi/poi-ooxml
libraryDependencies += "org.apache.poi" % "poi-ooxml" % "5.0.0"

// https://mvnrepository.com/artifact/com.microsoft.sqlserver/mssql-jdbc
libraryDependencies += "com.microsoft.sqlserver" % "mssql-jdbc" % "9.4.0.jre8"

// https://mvnrepository.com/artifact/com.itextpdf/itextpdf
libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.13.2"

// https://mvnrepository.com/artifact/com.itextpdf.tool/xmlworker
libraryDependencies += "com.itextpdf.tool" % "xmlworker" % "5.5.13.2"

// https://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "org.jsoup" % "jsoup" % "1.14.2"

mappings in Universal ++=
  (baseDirectory.value / "report_template" * "*" get) map
    (x => x -> ("report_template/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "notification" * "*" get) map
    (x => x -> ("notification/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "updateDB" * "*" get) map
    (x => x -> ("updateDB/" + x.getName))
        
mappings in Universal ++= 
 List(file("public/css/bootstrap.min.css") -> "public/css/bootstrap.min.css",
 	file("public/css/aqm.css") -> "public/css/aqm.css",
 	file("public/images/no_photo.png") -> "public/images/no_photo.png"
 )
     
PlayKeys.fileWatchService := play.runsupport.FileWatchService.sbt(2000)

scalacOptions += "-feature"

fork in run := false
