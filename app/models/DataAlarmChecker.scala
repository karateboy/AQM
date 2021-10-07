package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
import ModelHelper._
import models._
import models.ModelHelper._
import play.api.libs.json.Json

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

case object DataCheckFinish

class DataAlarmChecker extends Actor {
  def receive = {
    case Start(startTime) =>
      val checkFuture = check
      val parent = sender

      checkFuture.onComplete { 
        case Success(a)=>
          if (a)
            parent ! AlarmCheck
            
          parent ! DataCheckFinish
        case Failure(ex)=>
          Logger.error(ex.getMessage)
          parent ! DataCheckFinish
      }
  }

  def check = {
    Future {
      val alarm1 = checkMinData
      val alarm2 = checkHourData
      alarm1 || alarm2
    }
  }

  def checkMinData() = {
    var alarm = false
    for{m<-Monitor.mvList
      mCase = Monitor.map(m)
      autoAudit = mCase.autoAudit
      }{
        if(autoAudit.overInternalStdMinRule.isDefined){
          val rule = autoAudit.overInternalStdMinRule.get
          if(rule.checkInvalid(m))
            alarm = true
        }
        
        if(autoAudit.dataReadyMinRule.isDefined){
          val rule = autoAudit.dataReadyMinRule.get
          if(rule.checkInvalid(m))
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
              val ar = Alarm.Alarm(m, mt.toString, r._1.toDateTime, 1.0f, "011")
              try {
                if(Alarm.insertAlarm(ar) != 0){
                  Alarm.updateAlarmTicketState(ar.monitor, ar.mItem, new DateTime(ar.time), "ATO")
                  val ar_state =
                    if (ar.mVal == 0)
                      "恢復正常"
                    else
                      "觸發"

                  val reason = s"${ar.time.toString("YYYY/MM/dd HH:mm")} ${Monitor.map(ar.monitor).name}:${Alarm.map(ar.mItem)}-${MonitorStatus.map(ar.code).desp}:${ar_state}"
                  val mtOpt = try {
                    Some(MonitorType.withName(ar.mItem))
                  } catch {
                    case ex: Throwable =>
                      None
                  }
                  val (repairType, repairSubType) = if (mtOpt.isDefined) {
                    (Some("數據"), Some(MonitorType.map(mtOpt.get).desp))
                  } else
                    (None, None)

                  implicit val w1 = Json.writes[PartFormData]
                  implicit val write = Json.writes[RepairFormData]
                  val ticket =
                    Ticket(0, DateTime.now, true, TicketType.repair, 19,
                      SystemConfig.getAlarmTicketDefaultUserId(), ar.monitor, mtOpt, reason,
                      DateTime.now.plusDays(2), Json.toJson(Ticket.defaultAlarmTicketForm(ar)).toString,
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
    }

    alarm
  }
}