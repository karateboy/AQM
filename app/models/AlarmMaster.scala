package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
object StartCheck

class AlarmMaster extends Actor{
  var checkStartTime = DateTime.now
  def receive = {
    case StartCheck=>
      val worker = context.actorOf(Props[AlarmWorker], name = "alarmWorker")
      worker ! Start(checkStartTime)
    case Finish(endTime)=>
      checkStartTime = endTime
      context.stop(sender)
  }
}