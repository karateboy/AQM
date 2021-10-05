package models

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.nscala_time.time.Imports.DateTime
import models.ModelHelper._
import play.api.Play.current
import play.api._
import play.api.libs.concurrent.Akka
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

  def reloadEpaData(start:DateTime, end:DateTime): Unit = {
    receiver ! ReloadEpaData(start, end)
  }

  case object GetEpaHourData
  case class ReloadEpaData(start:DateTime, end:DateTime)
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

class OpenDataReceiver extends Actor with ActorLogging {

  import OpenDataReceiver._
  import com.github.nscala_time.time.Imports._

  val timer = {
    import scala.concurrent.duration._
    context.system.scheduler.schedule(Duration(5, SECONDS), Duration(1, HOURS), receiver, GetEpaHourData)
  }

  import scala.xml._

  def receive = {
    case GetEpaHourData =>
      val start = SystemConfig.getEpaLast - 7.day
      val end = DateTime.now().withMillisOfDay(0)
      if (start < end) {
        getEpaHourData(start, end)
      }

    case ReloadEpaData(start, end) =>
      Logger.info(s"reload EpaData ${start} ${end}")
      getEpaHourData(start, end)
  }

  def getEpaHourData(start: DateTime, end: DateTime) {
    Logger.info(s"get EPA data start=${start.toString()} end=${end.toString()}")
    val limit = 500

    def parser(node: Elem) = {
      import scala.collection.mutable.Map
      import scala.xml.Node
      val recordMap = Map.empty[EpaMonitor.Value, Map[DateTime, Map[MonitorType.Value, Double]]]

      def filter(dataNode: Node) = {
        val monitorDateOpt = dataNode \ "MonitorDate"
        val mDate = DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))
        start <= mDate && mDate < end
      }

      def processData(dataNode: Node) {
        val siteName = dataNode \ "SiteName"
        val itemId = dataNode \ "ItemId"
        val monitorDateOpt = dataNode \ "MonitorDate"
        val siteID = (dataNode \ "SiteId").text.trim.toInt

        try {
          //Filter interested EPA monitor
          if (EpaMonitor.idMap.contains(siteID) &&
            MonitorType.eapIdMap.contains(itemId.text.trim().toInt)) {
            val epaMonitor = EpaMonitor.withName(siteName.text.trim())
            val monitorType = MonitorType.eapIdMap(itemId.text.trim().toInt)
            val mDate = DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))

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

      qualifiedData.map {
        processData
      }

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
      if(updateCounts.sum != 0)
        Logger.info(s"EPA ${updateCounts.sum} records have been upserted.")

      qualifiedData.size
    }

    def getData(skip: Int) {
      val url = s"https://data.epa.gov.tw/api/v1/aqx_p_15?format=xml&offset=${skip}&limit=${limit}&api_key=fa3fdec2-19b2-4108-a7f0-63ea3a9a776a"
      val future =
        WS.url(url).get().map {
          response =>
            try {
              parser(response.xml)
            } catch {
              case ex: Exception =>
                Logger.error(ex.toString())
                throw ex
            }
        }
      future onFailure (errorHandler())
      future onSuccess ({
        case ret: Int =>
          Logger.info(s"EPA ${ret} records have been upserted.")
          if (ret < limit) {
            Logger.info(s"Import EPA ${start.toString()} to ${end} complete")
            SystemConfig.setEpaLast(end)
          } else
            getData(skip + limit)
      })
    }

    getData(0)
  }

  override def postStop = {
    timer.cancel()
  }

}

