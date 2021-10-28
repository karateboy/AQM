package models

import akka.actor._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models.Ozone8HrCalculator.updateCurrentOzone8Hr
import play.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.{Failure, Success}

case object DataCheckFinish

object AlarmDataType extends Enumeration {
  val Hour = Value("HOUR")
  val EightHour = Value("8Hour")
  val Day = Value("Day")
  val TwentyFourHour = Value("24H")
  val Year = Value("Year")

  def map(dataType: AlarmDataType.Value): String = {
    dataType match {
      case Hour =>
        "小時值"
      case EightHour =>
        "8小時平均值"
      case Day =>
        "日平均值"
      case TwentyFourHour =>
        "24小時值"
      case Year =>
        "年平均值"
    }
  }
}

case class AlarmLevel(level: AlarmLevel.Value, desc: String, code: Int)

object AlarmLevel extends Enumeration {
  val Internal = Value("Internal")
  val Warn = Value("Warn")
  val Law = Value("Law")

  val list = Seq(AlarmLevel(Internal, "內控值", 1),
    AlarmLevel(Warn, "警告值", 2),
    AlarmLevel(Law, "法規值", 4))

  val map: Map[AlarmLevel.Value, AlarmLevel] = list.map(p => p.level -> p).toMap

  val idMap: Map[Int, AlarmLevel] = list.map(p => p.code -> p).toMap
}

class DataAlarmChecker extends Actor {

  import AlarmMaster._

  def receive = {
    case Start(startTime) =>
      val checkFuture = check
      val parent = sender

      checkFuture.onComplete {
        case Success(a) =>
          if (a)
            parent ! AlarmCheck

          parent ! DataCheckFinish
        case Failure(ex) =>
          Logger.error(ex.getMessage, ex)
          parent ! DataCheckFinish
      }
  }

  def check = {
    Future {
      updateCurrentOzone8Hr
      val alarm1 = checkMinData
      val alarm2 = checkHourData
      alarm1 || alarm2
    }
  }

  def checkMinData() = {
    var alarm = false
    for {m <- Monitor.mvList
         mCase = Monitor.map(m)
         autoAudit = mCase.autoAudit
         } {
      if (autoAudit.overInternalStdMinRule.isDefined) {
        val rule = autoAudit.overInternalStdMinRule.get
        if (rule.checkInvalid(m))
          alarm = true
      }

      if (autoAudit.dataReadyMinRule.isDefined) {
        val rule = autoAudit.dataReadyMinRule.get
        if (rule.checkInvalid(m))
          alarm = true
      }
    }

    alarm
  }

  def checkHourData() = {
    var alarm = false
    val currentHour = Realtime.getLatestRecordTime(TableType.Hour).get

    for {
      m <- Monitor.mvList
      hours = Record.getUncheckedHourRecords(m, currentHour, currentHour.toDateTime + 1.hour) if hours.length >= 0
      mCase = Monitor.map(m)
    } {
      val otherRule = mCase.autoAudit.otherRule.getOrElse(OtherRule())
      for (mt <- mCase.monitorTypes) {
        val records = hours.map { h => (Record.timeProjection(h), Record.monitorTypeProject2(mt)(h)) }
        for ((time, (valueOpt, statusOpt)) <- records) {
          for {
            v <- valueOpt
            status <- statusOpt if MonitorStatus.isNormalStat(status)
          } {
            def checkStdLaw(): Option[Boolean] =
              for (stdLaw <- MonitorTypeAlert.map(m)(mt).std_law if v >= stdLaw) yield {
                alarm = true
                val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Law}"
                val ar = Alarm.Alarm(m, mItem, time.toDateTime, v, MonitorStatus.OVER_STAT)
                try {
                  if (otherRule.overStd)
                    Alarm.insertAlarm(ar)

                  for (autoTicket <- otherRule.autoTicket if autoTicket == true && otherRule.overStd2.getOrElse(true))
                    Alarm.newTicketFromAlarm(ar, DateTime.now.plusDays(2))
                } catch {
                  case _: Exception =>
                }
                true
              }

            def checkWarn(): Option[Boolean] =
              for (warn <- MonitorTypeAlert.map(m)(mt).warn if v >= warn) yield {
                alarm = true
                val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Warn}"
                val ar = Alarm.Alarm(m, mItem, time.toDateTime, v, MonitorStatus.WARN_STAT)
                try {
                  if (otherRule.overStd)
                    Alarm.insertAlarm(ar)

                  for (autoTicket <- otherRule.autoTicket if autoTicket == true && otherRule.overStd2.getOrElse(true))
                    Alarm.newTicketFromAlarm(ar, DateTime.now.plusDays(2))
                } catch {
                  case _: Exception =>
                }
                true
              }

            def checkInternal(): Option[Boolean] =
              for (std_internal <- MonitorTypeAlert.map(m)(mt).internal if v >= std_internal) yield {
                alarm = true
                val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Internal}"
                val ar = Alarm.Alarm(m, mItem, time.toDateTime, v, MonitorStatus.WARN_STAT)
                try {
                  if (otherRule.overStd)
                    Alarm.insertAlarm(ar)

                  for (autoTicket <- otherRule.autoTicket if autoTicket == true && otherRule.overStd2.getOrElse(true))
                    Alarm.newTicketFromAlarm(ar, DateTime.now.plusDays(2))
                } catch {
                  case _: Exception =>
                }
                true
              }

            val checkSeq: Seq[(Option[Float], () => Option[Boolean])] = Seq((MonitorTypeAlert.map(m)(mt).std_law, checkStdLaw),
              (MonitorTypeAlert.map(m)(mt).warn, checkWarn),
              (MonitorTypeAlert.map(m)(mt).internal, checkInternal))
            val sorted: Seq[(Option[Float], () => Option[Boolean])] = checkSeq.sortBy(_._1).reverse
            sorted.find(t => t._2() == Some(true))
          } // Normal status

          for {
            v <- valueOpt
            status <- statusOpt if MonitorStatus.isInvalidOrCalibration(status)
          } {
            alarm = true
            val ar = Alarm.Alarm(m, mt.toString, time.toDateTime, v, status)

            try {
              if ((status == "030" && otherRule.invalidData) ||
                (status == "026" && otherRule.calibrate) ||
                (status == "035" && otherRule.instAbnormal))
                Alarm.insertAlarm(ar)

              for (autoTicket <- otherRule.autoTicket if autoTicket == true &&
                ((status == "030" && otherRule.invalidData2.getOrElse(true)) ||
                  (status == "026" && otherRule.calibrate2.getOrElse(true)) ||
                  (status == "035" && otherRule.instAbnormal2.getOrElse(true))))
                Alarm.newTicketFromAlarm(ar, DateTime.now.plusDays(2))
            } catch {
              case _: Exception =>
            }
          }
        }
      }

      //Auto audit
      Auditor.auditHourData(m, mCase.autoAudit, currentHour.toDateTime - 1.day, currentHour.toDateTime + 1.hour)
      AggregateReport2.generate(m, hours)
    }

    alarm
  }
}