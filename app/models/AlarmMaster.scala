package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
import models.Realtime._
import ModelHelper._

object AlarmCheck
object DataCheck

class AlarmMaster extends Actor{
  var checkStartTime = DateTime.now
  def receive = {
    case AlarmCheck=>
        val worker = context.actorOf(Props[AlarmWorker], name = "alarmWorker" + (Math.random()*1000).toInt)
        worker ! Start(checkStartTime)
    case Finish(endTime)=>
      checkStartTime = endTime
      sender!PoisonPill
      
    case DataCheck=>
        val worker = context.actorOf(Props[DataAlarmChecker], name = "dataChecker" + (Math.random()*1000).toInt)
        worker ! Start(DateTime.now)
    case DataCheckFinish=>
      sender!PoisonPill
  }
}