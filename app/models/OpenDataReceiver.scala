package models

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.nscala_time.time.Imports.DateTime
import models.ModelHelper._
import play.api.Play.current
import play.api._
import play.api.libs.concurrent.Akka
import play.api.libs.functional.syntax.{functionalCanBuildApplicative, toFunctionalBuilderOps}
import play.api.libs.json.{JsError, JsPath, Json, Reads}
import play.api.libs.ws._
import scalikejdbc._

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global

object OpenDataReceiver {
  val props = Props[OpenDataReceiver]
  var receiver: ActorRef = _

  def startup() = {
    receiver = Akka.system.actorOf(props, name = "openDataReceiver")
    Logger.info(s"OpenData receiver starts")
  }

  def reloadEpaData(start: DateTime, end: DateTime): Unit = {
    receiver ! ReloadEpaData(start, end)
  }

  case class ReloadEpaData(start: DateTime, end: DateTime)

  case object GetEpaHourData

  case object GetEpaCurrentData
}

import java.util.Date

case class HourData(
                     SiteId: String,
                     SiteName: String,
                     ItemId: String,
                     ItemName: String,
                     ItemEngName: String,
                     ItemUnit: String,
                     MonitorDate: Date,
                     MonitorValues: Seq[Double])

// "SO2":"1","CO":"0.15","CO_8hr":"0.1","O3":"37.2","O3_8hr":"30",
// "PM10":"22","PM2.5":"7","NO2":"2.9","NOx":"4.1","NO":"1.2","WindSpeed":"2.7","WindDirec":"95"
case class EpaRecord(SiteName: String,
                     County: String,
                     PM10: Option[String],
                     PM25: Option[String],
                     SO2: Option[String],
                     CO: Option[String],
                     O3: Option[String],
                     NO2: Option[String],
                     NO: Option[String],
                     WindSpeed: Option[String],
                     WindDir: Option[String],
                     PublishTime: String,
                     SiteId: String)

case class EpaResult(records: Seq[EpaRecord])

case class MtRecord(SiteName: String, ItemId: String, Concentration: Option[String],
                    MonitorDate: String, SiteId: String)

case class MtResult(records: Seq[MtRecord])

class OpenDataReceiver extends Actor with ActorLogging {

  import OpenDataReceiver._
  import com.github.nscala_time.time.Imports._

  val timer = {
    import scala.concurrent.duration._
    context.system.scheduler.schedule(Duration(3, HOURS), Duration(3, HOURS), receiver, GetEpaHourData)
  }

  val timer1 = {
    import scala.concurrent.duration._
    context.system.scheduler.schedule(FiniteDuration(5, SECONDS), FiniteDuration(30, MINUTES), receiver, GetEpaCurrentData)
  }

  implicit val epaRecordReads: Reads[EpaRecord] = {
    val builder = {
      (JsPath \ "SiteName").read[String] and
        (JsPath \ "County").read[String] and
        (JsPath \ "PM10").readNullable[String] and
        (JsPath \ "PM2.5").readNullable[String] and
        (JsPath \ "SO2").readNullable[String] and
        (JsPath \ "CO").readNullable[String] and
        (JsPath \ "O3").readNullable[String] and
        (JsPath \ "NO2").readNullable[String] and
        (JsPath \ "NO").readNullable[String] and
        (JsPath \ "WindSpeed").readNullable[String] and
        (JsPath \ "WindDirec").readNullable[String] and
        (JsPath \ "PublishTime").read[String] and
        (JsPath \ "SiteId").read[String]
    }
    (builder) (EpaRecord.apply _)
  }

  import scala.xml._


  def receive = {
    case GetEpaHourData =>
      val start = SystemConfig.getEpaLast
      val end = DateTime.now().withMillisOfDay(0)
      if (start < end) {
        Logger.info(s"Get EpaData ${start.toString("yyyy-MM-d")} => ${end.toString("yyyy-MM-d")}")
        getEpaHourData(start, end)
      }

    case ReloadEpaData(start, end) =>
      Logger.info(s"reload EpaData ${start.toString("yyyy-MM-d")} => ${end.toString("yyyy-MM-d")}")
      getEpaHourData(start, end)

    case GetEpaCurrentData =>
      getCurrentCountyData("https://data.epa.gov.tw/api/v1/aqx_p_145?format=json&limit=500&api_key=9be7b239-557b-4c10-9775-78cadfc555e9")
      getCurrentCountyData("https://data.epa.gov.tw/api/v1/aqx_p_147?format=json&limit=500&api_key=9be7b239-557b-4c10-9775-78cadfc555e9")
      getCurrentCountyData("https://data.epa.gov.tw/api/v1/aqx_p_143?format=json&limit=500&api_key=9be7b239-557b-4c10-9775-78cadfc555e9")
  }

  def getCurrentData(limit: Int) = {
    import com.github.nscala_time.time.Imports._
    val url = s"https://data.epa.gov.tw/api/v1/aqx_p_432?format=json&limit=${limit}&api_key=9be7b239-557b-4c10-9775-78cadfc555e9"

    val f = WS.url(url).get()
    f onFailure (errorHandler(""))
    for (ret <- f) yield {
      var latestRecordTime = DateTime.now() - 1.day
      implicit val epaResultReads = Json.reads[EpaResult]
      val retEpaResult = ret.json.validate[EpaResult]

      def handleEpaRecords(records: Seq[EpaRecord]) {
        val filtered = records.filter(r => {
          try {
            val id = r.SiteId
            EpaMonitor.idMap.contains(id.toInt)
          } catch {
            case _: Throwable =>
              false
          }
        })
        filtered.
          foreach({
            record =>
              val id = record.SiteId
              val time = record.PublishTime
              val epaMonitor = EpaMonitor.idMap(id.toInt)
              val dt = DateTime.parse(time.trim(), DateTimeFormat.forPattern("YYYY/MM/dd HH:mm:ss"))
              if (latestRecordTime < dt)
                latestRecordTime = dt

              val overStdCode: Int = AlarmLevel.map(AlarmLevel.Internal).code

              def handle(epaTag: Option[String], mt: MonitorType.Value): Unit = {
                for (mtValueStr <- epaTag)
                  try {
                    val v = mtValueStr.toFloat
                    upsertEpaRecord(epaMonitor, mt, dt, v)
                    for (internal <- EpaMonitorTypeAlert.map(epaMonitor)(mt).internal if v >= internal)
                      EpaTicket.upsert(EpaTicket(dt, epaMonitor, mt, v, overStdCode))
                  } catch {
                    case _: Throwable =>
                  }
              }

              handle(record.PM25, MonitorType.A215)
              handle(record.PM10, MonitorType.A214)
              handle(record.SO2, MonitorType.A222)
              handle(record.CO, MonitorType.A224)
              handle(record.O3, MonitorType.A225)
              handle(record.NO2, MonitorType.A293)
              handle(record.NO, MonitorType.A283)
              handle(record.WindSpeed, MonitorType.C211)
              handle(record.WindDir, MonitorType.C212)
          })
      }

      retEpaResult.fold(
        err => {
          Logger.error(JsError.toJson(err).toString())
        },
        results => {
          try {
            handleEpaRecords(results.records)
          } catch {
            case ex: Exception =>
              Logger.error("failed to handled epaRecord", ex)
          }
        }
      )
    }
  }

  def upsertEpaRecord(m: EpaMonitor.Value, mt: MonitorType.Value, dateTime: DateTime, value: Double)
                     (implicit session: DBSession = AutoSession) = {
    val MStation = EpaMonitor.map(m)
    val MItem = MonitorType.map(mt).epa_mapping.get
    val MDate: java.sql.Timestamp = dateTime
    sql"""
              UPDATE dbo.hour_data
              SET MValue = ${value}
              WHERE MStation=${MStation.id} and MDate=${MDate} and MItem=${MItem};

              IF(@@ROWCOUNT = 0)
              BEGIN
                INSERT INTO dbo.hour_data (MStation, MDate, MItem, MValue)
                VALUES(${MStation.id}, ${MDate}, ${MItem}, ${value})
              END
            """.update.apply
  }

  def getCurrentCountyData(url: String) = {
    import com.github.nscala_time.time.Imports._
    val f = WS.url(url).get()
    f onFailure (errorHandler(""))
    for (ret <- f) yield {
      implicit val r = Json.reads[MtRecord]
      implicit val epaResultReads = Json.reads[MtResult]
      val retEpaResult = ret.json.validate[MtResult]

      def handleEpaRecords(records: Seq[MtRecord]) {
        val filtered = records.filter(r => {
          try {
            val id = r.SiteId
            EpaMonitor.idMap.contains(id.toInt)
          } catch {
            case _: Throwable =>
              false
          }
        })
        filtered.
          foreach({
            record =>
              val id = record.SiteId
              val time = record.MonitorDate
              val epaMonitor = EpaMonitor.idMap(id.toInt)
              val dt = try {
                DateTime.parse(time.trim(), DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))
              } catch {
                case ex: IllegalArgumentException =>
                  DateTime.parse(time.trim(), DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
              }
              val overStdCode: Int = AlarmLevel.map(AlarmLevel.Internal).code

              for (mtValueStr <- record.Concentration)
                try {
                  val v = mtValueStr.toFloat
                  val mt = MonitorType.eapIdMap(record.ItemId.toInt)
                  upsertEpaRecord(epaMonitor, mt, dt, v)
                  for (internal <- EpaMonitorTypeAlert.map(epaMonitor)(mt).internal if v >= internal)
                    EpaTicket.upsert(EpaTicket(dt, epaMonitor, mt, v, overStdCode))
                } catch {
                  case _: Throwable =>
                }
          })
      }

      retEpaResult.fold(
        err => {
          Logger.error(JsError.toJson(err).toString())
        },
        results => {
          try {
            handleEpaRecords(results.records)
          } catch {
            case ex: Exception =>
              Logger.error("failed to handled epaRecord", ex)
          }
        }
      )
    }
  }

  def getEpaHourData(start: DateTime, end: DateTime) {
    val limit = 500

    def parser(node: Elem) = {
      import scala.collection.mutable.Map
      import scala.xml.Node
      val recordMap = Map.empty[EpaMonitor.Value, Map[DateTime, Map[MonitorType.Value, Double]]]

      def filter(dataNode: Node) = {
        val monitorDateOpt = dataNode \ "MonitorDate"
        val mDate =
          try {
            DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))
          } catch {
            case _: Exception =>
              DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd"))
          }

        start <= mDate && mDate < end
      }

      def processData(dataNode: Node) {
        val siteName = dataNode \ "SiteName"
        val itemId = dataNode \ "ItemId"
        val monitorDateOpt = dataNode \ "MonitorDate"
        val siteID = try {
          (dataNode \ "SiteId").text.trim.toInt
        } catch {
          case _: Throwable =>
            // FIXME workaround EPA data bug!
            (dataNode \ "SiteId").text.trim.toDouble.toInt
        }

        try {
          //Filter interested EPA monitor
          if (EpaMonitor.idMap.contains(siteID) &&
            MonitorType.eapIdMap.contains(itemId.text.trim().toInt)) {
            val epaMonitor = EpaMonitor.withName(siteName.text.trim())
            val monitorType = MonitorType.eapIdMap(itemId.text.trim().toInt)
            val mDate = try {
              DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))
            } catch {
              case _: Exception =>
                DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd"))
            }

            val monitorNodeValueSeq =
              for (v <- 0 to 23) yield {
                val monitorValue = try {
                  Some((dataNode \ "MonitorValue%02d".format(v)).text.trim().toDouble)
                } catch {
                  case x: Throwable =>
                    None
                }
                (mDate + v.hour, monitorValue)
              }

            val timeMap = recordMap.getOrElseUpdate(epaMonitor, Map.empty[DateTime, Map[MonitorType.Value, Double]])
            for {(mDate, mtValueOpt) <- monitorNodeValueSeq} {
              val mtMap = timeMap.getOrElseUpdate(mDate, Map.empty[MonitorType.Value, Double])
              for (mtValue <- mtValueOpt)
                mtMap.put(monitorType, mtValue)
            }
          }
        } catch {
          case x: Throwable =>
            Logger.error("failed", x)
        }
      }

      val data = node \ "data"

      val qualifiedData = data.filter(filter)

      qualifiedData.foreach(node => {
        try {
          processData(node)
        } catch {
          case _: Throwable =>
          //skip buggy data
        }
      })

      val updateCounts =
        for {
          monitorMap <- recordMap
          monitor = monitorMap._1
          timeMaps = monitorMap._2
          dateTime <- timeMaps.keys.toList.sorted
          mtValue <- timeMaps(dateTime)
        } yield {
          val MStation = EpaMonitor.map(monitor)
          val MItem = MonitorType.map(mtValue._1).epa_mapping.get
          val MDate: java.sql.Timestamp = dateTime
          DB autoCommit { implicit session =>
            sql"""
              UPDATE dbo.hour_data
              SET MValue = ${mtValue._2}
              WHERE MStation=${MStation.id} and MDate=${MDate} and MItem=${MItem};

              IF(@@ROWCOUNT = 0)
              BEGIN
                INSERT INTO dbo.hour_data (MStation, MDate, MItem, MValue)
                VALUES(${MStation.id}, ${MDate}, ${MItem}, ${mtValue._2})
              END
            """.update.apply
          }
        }

      qualifiedData.size
    }

    def getThisMonth(skip: Int) {
      val url = s"https://data.epa.gov.tw/api/v1/aqx_p_15?format=xml&offset=${skip}&limit=${limit}&api_key=fa3fdec2-19b2-4108-a7f0-63ea3a9a776a"
      val future =
        WS.url(url).get().map {
          response =>
            try {
              parser(response.xml)
            } catch {
              case ex: Exception =>
                Logger.error(ex.toString(), ex)
                throw ex
            }
        }
      future onFailure (errorHandler())
      future onSuccess ({
        case ret: Int =>
          if (ret < limit) {
            Logger.info(s"Import EPA ${start.getYear()}/${start.getMonthOfYear()} complete")
            LineNotify.notify("環保署資料擷取成功.")
          } else
            getThisMonth(skip + limit)
      })
    }

    def getMonthData(year: Int, month: Int, skip: Int) {
      val url = f"https://data.epa.gov.tw/api/v1/aqx_p_15?format=xml&offset=$skip%d&limit=$limit&year_month=$year%d_$month%02d&api_key=fa3fdec2-19b2-4108-a7f0-63ea3a9a776a"
      val f = WS.url(url).get()
      f onFailure (errorHandler())
      for (resp <- f) {
        try {
          val updateCount = parser(resp.xml)
          if (updateCount < limit) {
            Logger.info(f"Import EPA $year/$month%02d complete")
            val dataLast = new DateTime(year, month, 1, 0, 0).plusMonths(1)
            SystemConfig.setEpaLast(dataLast)
            if (dataLast < end)
              self ! ReloadEpaData(dataLast, end)
            else
              LineNotify.notify("環保署資料回補完成.")
          } else
            getMonthData(year, month, skip + limit)
        } catch {
          case ex: Exception =>
            Logger.error(ex.toString(), ex)
            throw ex
        }
      }
    }

    if (start.toString("yyyy-M") == DateTime.now().toString("yyyy-M"))
      getThisMonth(0)
    else {
      getMonthData(start.getYear(), start.getMonthOfYear(), 0)
    }
  }

  // NMHC https://data.epa.gov.tw/dataset/aqx_p_313
  // THC https://data.epa.gov.tw/dataset/aqx_p_312
  // 溫度 相對濕度 https://data.epa.gov.tw/dataset/aqx_p_35/resource/682534c9-85f8-4525-a6ca-1a3699d892a0
  // 雨量 https://data.epa.gov.tw/dataset/aqx_p_35/resource/682534c9-85f8-4525-a6ca-1a3699d892a0

  override def postStop = {
    timer.cancel()
    timer1.cancel()
  }

}

