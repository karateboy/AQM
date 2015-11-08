package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
object StartCheck

class AlarmMaster extends Actor{
  var checkStartTime = DateTime.now
  var checking = false
  def receive = {
    case StartCheck=>
      if(!checking){
        val worker = context.actorOf(Props[AlarmWorker], name = "alarmWorker")
        worker ! Start(checkStartTime)
        checking = true
      }      
    case Finish(endTime)=>
      checkStartTime = endTime
      checking = false
      context.stop(sender)
  }
}