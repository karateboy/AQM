package models
import play.api._
import play.api.mvc._
import models.ModelHelper._

import scala.collection.Map
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import EnumUtils._
import play.api.libs.json.Json

object SystemConfig {
  val AutoAuditAsNormal = "AutoAuditAsNormal"
  val DownloadLink = "DownloadLink"

  private val MaxFileLink = 10
  def getFreeDownloadLink(): Int = {
    for (i <- 0 to MaxFileLink) {
      val link = SystemConfig.getConfig(DownloadLink + i, "")
      if (link.length() == 0)
        return i
    }

    0
  }

  import java.io.File
  def setDownloadLink(n: Int, file: File) {
    assert(n <= MaxFileLink)
    SystemConfig.setConfig(DownloadLink + n, file.getAbsolutePath)
  }
  def cleanDownloadLink(n: Int) {
    assert(n <= MaxFileLink)
    SystemConfig.setConfig(DownloadLink + n, "")
  }

  def getDownloadLink(n: Int) = {
    assert(n <= MaxFileLink)
    val path = SystemConfig.getConfig(DownloadLink + n, "")
    new File(path)
  }

  val dtPattern = "YYYY-MM-dd HH:mm"
  val AlarmCheckPointKey = "AlarmCheckPoint"
  def getAlarmCheckPoint() = {
    val checkPoint = SystemConfig.getConfig(AlarmCheckPointKey, "2016-05-03 00:00")
    DateTime.parse(checkPoint, DateTimeFormat.forPattern(dtPattern))
  }

  def setAlarmCheckPoint(time: DateTime) = {
    val checkPoint = time.toString(dtPattern)
    SystemConfig.setConfig(AlarmCheckPointKey, checkPoint)
  }

  val AlarmTicketDefaultUserIdKey = "AlarmTicketDefaultUserId"
  def getAlarmTicketDefaultUserId() = SystemConfig.getConfig(AlarmTicketDefaultUserIdKey, "5").toInt
  def setAlarmTicketDefaultUserId(id: Int) =
    SystemConfig.setConfig(AlarmTicketDefaultUserIdKey, id.toString)

  val PM10ThresholdKey = "PM10Threshold"
  def getPM10Threshold() = SystemConfig.getConfig(PM10ThresholdKey, "950").toDouble
  def setPM10Threshold(v: Double) = SystemConfig.setConfig(PM10ThresholdKey, v.toString())

  val EpaLast = "EpaLast"
  def getEpaLast = DateTime.parse(SystemConfig.getConfig(EpaLast, "2019-1-1 00:00"), DateTimeFormat.forPattern("yyyy-MM-dd HH:mm"))
  def setEpaLast(dateTime: DateTime) = SystemConfig.setConfig(EpaLast, dateTime.toString("yyyy-MM-dd HH:mm"))
  var map = {
    val configPair =
      DB readOnly {
        implicit session =>

          sql"""
        Select * 
        From SystemConfig
        """.map { r => (r.string(1) -> r.string(2)) }.list.apply
      }
    Map(configPair: _*)
  }

  val Ozone8HrCalculateDate = "Ozone8HrCalculateDate"
  def getOzone8HrCalculateDate = DateTime.parse(SystemConfig.getConfig(Ozone8HrCalculateDate, DateTime.now.toString("yyyy-MM-dd")),
    DateTimeFormat.forPattern("yyyy-MM-dd"))
  def setOzone8HrCalculateDate(dateTime: DateTime) = SystemConfig.setConfig(Ozone8HrCalculateDate, dateTime.toString("yyyy-MM-dd"))

  def getConfig(key: String, defaultValue: String) = {
    val opt = map.get(key)
    if (opt.isDefined)
      opt.get
    else {
      newConfig(key, defaultValue)
      defaultValue
    }
  }

  val ConvertOverStdYear = "ConvertOverStdYear"
  def getConvertOverStdYear = SystemConfig.getConfig(ConvertOverStdYear, "2021").toInt
  def setConvertOverStdYear(year:Int) = SystemConfig.setConfig(ConvertOverStdYear, year.toString)

  val extendedReasons = "ExtendedReason"
  def getExtendedReasons: Seq[String] = {
    val jsonStr = SystemConfig.getConfig(extendedReasons, """["其他"]""")
    Json.parse(jsonStr).validate[Seq[String]].getOrElse(Seq.empty[String])
  }
  def setExtededReasons(selections: Seq[String]) = {
    val ret = Json.toJson(selections).toString()
    SystemConfig.setConfig(extendedReasons, ret)
  }

  val GenerateAggregate2 = "GenerateAggreate2"
  def getGenerateAggregate2 = SystemConfig.getConfig(GenerateAggregate2, true.toString).toBoolean
  def setGenerateAggregate2(value: Boolean) = SystemConfig.setConfig(GenerateAggregate2, value.toString)

  val UpdatePastAggregate2 = "UpdatePastAggregate2"
  def getUpdatePastAggregate2 = SystemConfig.getConfig(UpdatePastAggregate2, true.toString).toBoolean
  def setUpdatePastAggregate2(value: Boolean) = SystemConfig.setConfig(UpdatePastAggregate2, value.toString)

  def setConfig(key: String, value: String) = {
    map = (map - key) + (key -> value)
    DB localTx {
      implicit session =>
        sql"""
          UPDATE SystemConfig
          SET [value] = ${value}
          WHERE configKey = ${key.toString}
          """.update.apply
    }
  }

  def newConfig(key: String, value: String) = {
    DB localTx {
      implicit session =>
        sql"""
          INSERT INTO SystemConfig([configKey],[value])
          VALUES (${key}, ${value})
          """.update.apply
    }
    map += (key -> value)
  }
}