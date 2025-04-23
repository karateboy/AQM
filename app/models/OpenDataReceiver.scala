package models

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.nscala_time.time.Imports.DateTime
import models.ModelHelper._
import play.api.Play.current
import play.api._
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsError, Json}
import play.api.libs.ws._
import scalikejdbc._

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

case class MtRecord(sitename: String, itemid: String, concentration: Option[String],
                    monitordate: String, siteid: String)

case class MtResult(records: Seq[MtRecord])

class OpenDataReceiver extends Actor with ActorLogging {

  import OpenDataReceiver._
  import com.github.nscala_time.time.Imports._

  val timer = {
    import scala.concurrent.duration._
    context.system.scheduler.schedule(Duration(1, HOURS), Duration(1, DAYS), receiver, GetEpaHourData)
  }

  val timer1 = {
    import scala.concurrent.duration._
    context.system.scheduler.schedule(FiniteDuration(5, SECONDS), FiniteDuration(30, MINUTES), receiver, GetEpaCurrentData)
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
      getCurrentCountyData("https://data.moenv.gov.tw/api/v2/aqx_p_145?format=json&limit=500&api_key=1f4ca8f8-8af9-473d-852b-b8f2d575f26a&sort=MonitorDate desc")
      getCurrentCountyData("https://data.moenv.gov.tw/api/v2/aqx_p_147?format=json&limit=500&api_key=1f4ca8f8-8af9-473d-852b-b8f2d575f26a&&sort=MonitorDate%20desc")
      getCurrentCountyData("https://data.moenv.gov.tw/api/v2/aqx_p_143?format=json&limit=500&api_key=1f4ca8f8-8af9-473d-852b-b8f2d575f26a&&sort=MonitorDate%20desc")
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
    f onFailure errorHandler("")
    for (ret <- f) yield {
      implicit val r = Json.reads[MtRecord]
      implicit val epaResultReads = Json.reads[MtResult]
      val retEpaResult = ret.json.validate[MtResult]

      def handleEpaRecords(records: Seq[MtRecord]) {
        val filtered = records.filter(r => {
          try {
            val id = r.siteid
            EpaMonitor.idMap.contains(id.toInt)
          } catch {
            case _: Throwable =>
              false
          }
        })
        filtered.
          foreach({
            record =>
              val id = record.siteid
              val time = record.monitordate
              val epaMonitor = EpaMonitor.idMap(id.toInt)
              val dt = try {
                DateTime.parse(time.trim(), DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))
              } catch {
                case ex: IllegalArgumentException =>
                  DateTime.parse(time.trim(), DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
              }
              val overStdCode: Int = AlarmLevel.map(AlarmLevel.Internal).code

              for (mtValueStr <- record.concentration)
                try {
                  val v = mtValueStr.toFloat
                  val mt = MonitorType.eapIdMap(record.itemid.toInt)
                  upsertEpaRecord(epaMonitor, mt, dt, v)
                  for (internal <- EpaMonitorTypeAlert.map(epaMonitor)(mt).internal if v >= internal) {
                    EpaTicket.upsert(EpaTicket(dt, epaMonitor, mt, v, overStdCode))
                  }
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

        val monitorDateOpt = dataNode \ "MonitorDate".toLowerCase()
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
        val siteName = dataNode \ "SiteName".toLowerCase()
        val itemId = dataNode \ "ItemId".toLowerCase()
        val monitorDateOpt = dataNode \ "MonitorDate".toLowerCase()
        val siteID = try {
          (dataNode \ "SiteId".toLowerCase()).text.trim.toInt
        } catch {
          case _: Throwable =>
            // FIXME workaround EPA data bug!
            (dataNode \ "SiteId".toLowerCase()).text.trim.toDouble.toInt
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
                  Some((dataNode \ "MonitorValue%02d".format(v).toLowerCase()).text.trim().toDouble)
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
      val url = s"https://data.moenv.gov.tw/api/v2/aqx_p_15?format=xml&offset=${skip}&limit=${limit}&api_key=1f4ca8f8-8af9-473d-852b-b8f2d575f26a&&sort=MonitorDate%20desc"
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
            val msg = "環境部資料擷取成功."
            LineNotify.notify(msg)
            EventLog.create(EventLog(DateTime.now(), EventLog.evtTypeGetEpaData, msg))
          } else
            getThisMonth(skip + limit)
      })
    }

    def getMonthData(year: Int, month: Int, skip: Int) {
      val url = f"https://data.moenv.gov.tw/api/v2/aqx_p_15?format=xml&offset=$skip%d&limit=$limit&year_month=$year%d_$month%02d&api_key=1f4ca8f8-8af9-473d-852b-b8f2d575f26a"
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
              LineNotify.notify("環境部資料回補完成.")
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

  // NMHC https://data.moenv.gov.tw/dataset/aqx_p_313
  // THC https://data.moenv.gov.tw/dataset/aqx_p_312
  // 溫度 相對濕度 https://data.moenv.gov.tw/dataset/aqx_p_35/resource/682534c9-85f8-4525-a6ca-1a3699d892a0
  // 雨量 https://data.moenv.gov.tw/dataset/aqx_p_35/resource/682534c9-85f8-4525-a6ca-1a3699d892a0

  override def postStop = {
    timer.cancel()
    timer1.cancel()
  }

}

