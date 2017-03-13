package models
import play.api._
import akka.actor._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import ModelHelper._
import com.github.nscala_time.time.Imports._
import scala.concurrent.ExecutionContext.Implicits.global

object PartAlarmWorker {
  case object CheckUsage
  def start() = {
    Akka.system.actorOf(Props[PartAlarmWorker], name = "PartAlarmWorker")
  }
}

class PartAlarmWorker extends Actor {
  import PartAlarmWorker._

  var timer = {

    Akka.system.scheduler.scheduleOnce(scala.concurrent.duration.Duration(3, scala.concurrent.duration.SECONDS), self, CheckUsage)
  }
  def receive = {
    case CheckUsage =>
      try {
        val partUsageList = PartUsage.getList()
        for (partUsage <- partUsageList if partUsage.getNextReplaceDate < DateTime.now) {
          val ar = Alarm.Alarm(Monitor.withName(partUsage.monitor), partUsage.getAlarmCode, DateTime.now, 1.0f, "055")
          try {
            Alarm.insertAlarm(ar)
            PartUsage.updateStartDate(partUsage.id, partUsage.getNextReplaceDate)            
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }
        }
      } catch {
        case ex: Throwable =>
          Logger.error("failed to check part usage!", ex)
      }
      timer = Akka.system.scheduler.scheduleOnce(scala.concurrent.duration.Duration(5, scala.concurrent.duration.MINUTES), self, CheckUsage)
  }

  override def postStop(): Unit = {
    timer.cancel()
  }
}