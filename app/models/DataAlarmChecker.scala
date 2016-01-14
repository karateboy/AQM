package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
import ModelHelper._
import models._
import models.ModelHelper._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

object DataCheckFinish

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
          if (r._2._1.isDefined && r._2._2.isDefined && mCase.getStdInternal(mt).isDefined) {
            val v = r._2._1.get
            val status = r._2._2.get
            val std_internal = mCase.getStdInternal(mt).get
            if (MonitorStatus.isNormalStat(status)
              && v > std_internal) {
              alarm = true
              val ar = Alarm.Alarm(m, mt.toString, r._1.toDateTime, 1.0f, "011")
              try {
                Alarm.insertAlarm(ar)
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