package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
import ModelHelper._
import models._

object DataCheckFinish

class DataAlarmChecker extends Actor{
   def receive = {
    case Start(startTime)=>
      sender ! checkHourData(startTime) // perform the work
  }
   
  def checkHourData(startTime:DateTime)={
    for{m <- Monitor.mvList
      hours = Record.getHourRecords(m, startTime - 1.hour, startTime) if hours.length >=0
      mCase = Monitor.map(m)
    }{ 
      for(mt <- mCase.monitorTypes){
          val records = hours.map { h=>(Record.timeProjection(h), Record.monitorTypeProject2(mt)(h))}
          for(r <- records){
            if(r._2._1.isDefined && r._2._2.isDefined && mCase.getStdInternal(mt).isDefined){
              val v = r._2._1.get
              val status = r._2._2.get
              val std_internal = mCase.getStdInternal(mt).get
                if(MonitorStatus.isNormalStat(status) 
                    && v > std_internal){
                      val ar = Alarm.Alarm(m, mt.toString, r._1.toDateTime, 1.0f, "011")
                      Alarm.insertAlarm(ar)
                }
            }
          }
      }
      //Auto audit
      Auditor.auditHourData(m, mCase.autoAudit, startTime - 1.hour, startTime)
    }
    
    DataCheckFinish
  }
}