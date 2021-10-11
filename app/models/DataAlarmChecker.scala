package models

import akka.actor._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models.Ozone8HrCalculator.updateCurrentOzone8Hr
import play.api._
import play.api.libs.json.Json

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

case class AlarmLevel(name: String, desc:String, code: Int)

object AlarmLevel extends Enumeration {
  val Internal = Value("Internal")
  val Warn = Value("Warn")
  val Law = Value("Law")
  val map = Map(Internal -> AlarmLevel(Internal.toString, "內控值",1),
    Warn -> AlarmLevel(Warn.toString, "警告值", 2),
    Law -> AlarmLevel(Law.toString, "法規值", 4)
  )
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
      for (mt <- mCase.monitorTypes) {
        val records = hours.map { h => (Record.timeProjection(h), Record.monitorTypeProject2(mt)(h)) }
        for (r <- records) {
          if (r._2._1.isDefined && r._2._2.isDefined && MonitorTypeAlert.map(m)(mt).internal.isDefined) {
            val v = r._2._1.get
            val status = r._2._2.get
            val std_internal = MonitorTypeAlert.map(m)(mt).internal.get
            if (MonitorStatus.isNormalStat(status)
              && v > std_internal) {
              alarm = true
              val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Internal}"
              val ar = Alarm.Alarm(m, mItem, r._1.toDateTime, v, MonitorStatus.OVER_STAT)
              try {
                Alarm.insertAlarm(ar)
              } catch {
                case ex: Exception =>
                // Skip duplicate alarm
              }
            }

            if(MonitorStatus.isInvalidOrCalibration(status)){
              val ar = Alarm.Alarm(m, mt.toString, r._1.toDateTime, v, status)
              try {
                if(Alarm.insertAlarm(ar)!=0){
                  Alarm.updateAlarmTicketState(m, mt.toString, r._1.toDateTime, "YES")
                  // Auto to ticket
                  val reason = s"${ar.time.toString("YYYY/MM/dd HH:mm")} ${Monitor.map(ar.monitor).name}:${Alarm.getItem(ar)}-${Alarm.getReason(ar)}:觸發"
                  val (repairType, repairSubType) =
                    (Some("數據"), Some(MonitorType.map(mt).desp))

                  val executeDate = DateTime.tomorrow().plusDays(1)
                  implicit val w1 = Json.writes[PartFormData]
                  implicit val write = Json.writes[RepairFormData]
                    val ticket =
                      Ticket(0, DateTime.now, true, TicketType.repair, 19,
                        SystemConfig.getAlarmTicketDefaultUserId(), m, Some(mt), reason,
                        executeDate, Json.toJson(Ticket.defaultAlarmTicketForm(ar)).toString,
                        repairType, repairSubType, Some(false))
                    Ticket.newTicket(ticket)
                  }
              } catch {
                case ex: Exception =>
                // Skip duplicate alarm
              }
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