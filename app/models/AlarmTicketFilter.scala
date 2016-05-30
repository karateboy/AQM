package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.concurrent.Akka

object AlarmTicketFilter {
  case object Check

  def start = {
    val worker = Akka.system.actorOf(Props[AlarmTicketFilter], name = "AlarmTicketFilter")
  }
}

class AlarmTicketFilter extends Actor {
  import AlarmTicketFilter._
  var timer = Akka.system.scheduler.scheduleOnce(scala.concurrent.duration.Duration(5, scala.concurrent.duration.SECONDS), self, Check)

  def receive = handler(None)

  def handler(checkPointOpt: Option[DateTime]): Receive = {
    case Check =>
      val checkPoint = checkPointOpt.getOrElse(DateTime.now() - 3.day)
      try {
        alarmFilter(checkPoint)
      } catch {
        case ex: Throwable =>
          Logger.error("fail to filter alarm", ex)
      }
  }

  def alarmFilter(checkPoint: DateTime) = {
    val now = DateTime.now
    val alarms = Alarm.getAlarmNoTicketList(checkPoint)
    for(ar <- alarms){
      if(ar.code == MonitorStatus.REPAIR || ar.code == MonitorStatus.MAINTANCE_STAT)
        Alarm.updateAlarmTicketState(ar.monitor, ar.mItem, ar.time, "PAS")
      else{
        val countOpt = findSameAlarm(ar.monitor, ar.mItem, ar.code)(ar.time - 30.minute, ar.time)
        if(countOpt.isDefined && countOpt.get != 0){
          Alarm.updateAlarmTicketState(ar.monitor, ar.mItem, ar.time, "PAS")
        }
      }
    }
    
    context become handler(Some(now))
    timer = Akka.system.scheduler.scheduleOnce(scala.concurrent.duration.Duration(5, scala.concurrent.duration.MINUTES), self, Check)
  }

  override def postStop() {
    timer.cancel()
  }

}