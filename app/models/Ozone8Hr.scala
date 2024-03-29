package models

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import models.ModelHelper._
import models.Record.HourRecord
import play.api.Logger
import play.api.libs.concurrent.Akka
import scalikejdbc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}

object Ozone8Hr {
  def createTab(year: Int)(implicit session: DBSession = AutoSession): Boolean = {
    val tabName: SQLSyntax = getTabName(year)
    val pkName = SQLSyntax.createUnsafely(s"PK_O3_8Hr_${year}")
    sql"""CREATE TABLE [dbo].[${tabName}](
            [DP_NO] [varchar](6) NOT NULL,
            [M_DateTime] [datetime2](7) NOT NULL,
            [Value] [float] NULL,
            [Status] [varchar](3) NOT NULL,
            CONSTRAINT [${pkName}] PRIMARY KEY CLUSTERED
            (
              [DP_NO] ASC,
              [M_DateTime] ASC
            )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
       ) ON [PRIMARY]""".execute().apply()
  }

  def getTabName(year: Int): SQLSyntax = SQLSyntax.createUnsafely(s"O3_8Hr_${year}")

  def hasOzone8hrTab(year: Int)(implicit session: DBSession = AutoSession): Boolean = {
    val list = {
      sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
         """.map { rs => rs.string(1) }.list().apply()
    }
    list.contains(s"O3_8Hr_${year}")
  }

  def hasHourTab(year: Int)(implicit session: DBSession = AutoSession): Boolean = {
    val list = {
      sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
         """.map { rs => rs.string(1) }.list().apply()
    }
    //P1234567_Hr_2012
    list.contains(s"P1234567_Hr_${year}")
  }
}

import com.github.nscala_time.time.Imports._
import play.api.Play.current

case class OzoneRecord(time: DateTime, value: Option[Float], status: Option[String])

object Ozone8HrCalculator {
  var worker: ActorRef = _

  def start = {
    val date = SystemConfig.getOzone8HrCalculateDate
    worker = Akka.system.actorOf(Props[Ozone8HrCalculator], name = "Ozone8HrCalculator")
    worker ! CalculateOzone(date)
  }

  def updateCurrentOzone8Hr(): Unit = {
    for (m <- Monitor.mvList) {
      if (Monitor.map(m).monitorTypes.contains(MonitorType.A225)) {
        val now = DateTime.now
        val hrList = Record.getHourRecords(m, DateTime.now - 8.hour, now)

        val recordTime = DateTime.now.withMillisOfDay(0).withHourOfDay(now.getHourOfDay)
        if (hrList.nonEmpty)
          calculateOzone8Hr(m.toString, hrList, recordTime)
      }
    }
  }

  private def isValidOzoneRecord(r: OzoneRecord): Boolean = {
    val validCodes = List("10", "11", "16", "22")
    r.status.isDefined && validCodes.contains(r.status.get.takeRight(2))
  }
  private def calculateOzone8Hr(DP_NO: String, hourRecordList: List[HourRecord], recordTime: DateTime): Unit = {
    if (!Ozone8Hr.hasOzone8hrTab(recordTime.getYear))
      Ozone8Hr.createTab(recordTime.getYear)

    val tabName = Ozone8Hr.getTabName(recordTime.getYear)
    val dataList: List[OzoneRecord] = hourRecordList
      .map(hr => OzoneRecord(hr.date.toJodaDateTime, hr.o3, hr.o3_stat))

    val avgList = dataList.filter(r=>isValidOzoneRecord(r)).flatMap(_.value)
    val avg: Option[Float] =
      if (avgList.length >= 5)
        Some(avgList.sum / avgList.length)
      else
        None

    if (avg.nonEmpty) {
      val statusMap: Map[Option[String], Int] = dataList.groupBy(_.status).mapValues(_.size)
      val status = statusMap.maxBy(t => t._2)._1.getOrElse("032")

      DB autoCommit { implicit session =>
        sql"""
              UPDATE $tabName
              SET [DP_NO] = $DP_NO
                  ,[M_DateTime] = $recordTime
                  ,[Value] = $avg
                  ,[Status] = ${status}
              WHERE [DP_NO]=$DP_NO and [M_DateTime] =$recordTime;

              IF(@@ROWCOUNT = 0)
              BEGIN
                INSERT INTO $tabName ([DP_NO], [M_DateTime], [Value], [Status])
                VALUES($DP_NO, $recordTime, $avg, ${status})
              END
            """.update.apply
      }
    } else {
      DB autoCommit { implicit session =>
        sql"""
              UPDATE $tabName
              SET [DP_NO] = $DP_NO
                  ,[M_DateTime] = $recordTime
                  ,[Value] = NULL
                  ,[Status] = '032'
              WHERE [DP_NO]=$DP_NO and [M_DateTime] =$recordTime;

              IF(@@ROWCOUNT = 0)
              BEGIN
                INSERT INTO $tabName ([DP_NO], [M_DateTime], [Value], [Status])
                VALUES($DP_NO, $recordTime, NULL, '032')
              END
            """.update.apply
      }
    }
  }

  private def calculateOzoneOfTheSameMonthBatch(start: DateTime, end: DateTime): Unit = {
    Logger.info(s"upsert O3 8hr ${start.getYear}/${start.getMonthOfYear}")
    val tabName = Ozone8Hr.getTabName(start.getYear)
    for (m <- Monitor.mvList) {
      if (Monitor.map(m).monitorTypes.contains(MonitorType.A225)) {
        val hrList: List[OzoneRecord] = Record.getHourRecords(m, start, end)
          .map(hr => OzoneRecord(hr.date.toJodaDateTime, hr.o3, hr.o3_stat)).toList
        val DP_NO = Monitor.map(m).id
        val avgList: Seq[List[OzoneRecord]] =
          for (i <- 0 to hrList.length - 8) yield
            hrList.slice(i, i + 8)


        val updateParamOptions: Seq[Option[Seq[Any]]] =
          for {avgData <- avgList if avgData.nonEmpty
               head = avgData.head
               dataList = avgData.filter(r => head.time <= r.time && r.time < head.time + 8.hour) if dataList.nonEmpty
               avgList = dataList.filter(isValidOzoneRecord).flatMap(_.value)
               } yield {
            val recordTime: java.sql.Timestamp = head.time + 7.hour
            val avg: Option[Float] = if (avgList.length >= 5)
              Some(avgList.sum / avgList.length)
            else
              None

            val statusMap: Map[Option[String], Int] = dataList.groupBy(_.status).mapValues(_.size)
            val status: String = statusMap.maxBy(t => t._2)._1.getOrElse("032")

            Some(Seq(DP_NO, recordTime, avg, status, DP_NO, recordTime, DP_NO, recordTime, avg, status))
          }
        val updateParam: Seq[Seq[Any]] = updateParamOptions.flatten
        DB autoCommit { implicit session =>
          sql"""
              UPDATE $tabName
              SET [DP_NO] = ?
                  ,[M_DateTime] = ?
                  ,[Value] = ?
                  ,[Status] = ?
              WHERE [DP_NO]=? and [M_DateTime] =?;

              IF(@@ROWCOUNT = 0)
              BEGIN
                INSERT INTO $tabName ([DP_NO], [M_DateTime], [Value], [Status])
                VALUES(?, ?, ?, ?)
              END
            """.batch(updateParam: _*).apply()
        }
      }
    }

  }

  private case class CalculateOzone(date: DateTime)

  private case object CalculateCurrent
}

class Ozone8HrCalculator extends Actor {

  import Ozone8HrCalculator._

  val timer: Cancellable = {
    import scala.concurrent.duration._
    Akka.system.scheduler.schedule(Duration(5, SECONDS), Duration(10, MINUTES), self, CalculateCurrent)
  }

  override def receive: Receive = {
    case CalculateCurrent =>
      if (LocalTime.now.getHourOfDay >= 8) {
        Future {
          blocking {
            calculateCurrent()
          }
        }
      }
    case CalculateOzone(date) =>
      /*
      if (!Ozone8Hr.hasHourTab(date.getYear)) {
        Logger.info(s"Ozone 8hr reach end of beginning year ${date.getYear}")
      } else {
        if (!Ozone8Hr.hasOzone8hrTab(date.getYear))
          Ozone8Hr.createTab(date.getYear)

        val start: DateTime = date.withDayOfMonth(1).withMillisOfDay(0)
        val end: DateTime = date.plusMonths(1).withMillisOfDay(0)

        Future {
          blocking {
            calculateOzoneOfTheSameMonthBatch(start, end)
            val nextStep = start.minusMonths(1)
            SystemConfig.setOzone8HrCalculateDate(nextStep)
            self ! CalculateOzone(nextStep)
          }
        }
      }*/
      val start: DateTime = date.withDayOfMonth(1).withMillisOfDay(0)
      val end: DateTime = DateTime.yesterday().withMillisOfDay(0)
      Future {
        blocking {
          calculateOzoneOfTheSameMonthBatch(start, end)
        }
      }
  }

  private def calculateCurrent(): Unit = {
    val now = DateTime.now
    val start: DateTime = now.withDayOfMonth(1).withMillisOfDay(0)
    val end: DateTime = now.plusMonths(1).withMillisOfDay(0)

    calculateOzoneOfTheSameMonthBatch(start, end)
    /*
    for (m <- Monitor.mvList if Monitor.map(m).monitorTypes.contains(MonitorType.A225)) {
      calculateOzoneOfTheSameMonthBatch()
      val hr = DateTime.now.getHourOfDay
      val end = DateTime.now().withMillisOfDay(0).withHourOfDay(hr)
      val start = end - 4.days
      val records = Record.getHourRecords(m, start, end)
      calculateOzone8Hr(Monitor.map(m).id, records, end - 1.hour)
    }*/
  }

  override def postStop(): Unit = {
    timer.cancel()
    super.postStop()
  }
}
