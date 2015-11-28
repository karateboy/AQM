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
  var checking = false
  var dataCheckingTime = DateTime.now
  var dataChecking = false
  def receive = {
    case AlarmCheck=>
      if(!checking){
        val worker = context.actorOf(Props[AlarmWorker], name = "alarmWorker" + (Math.random()*1000).toInt)
        worker ! Start(checkStartTime)
        checking = true
      } 
    case Finish(endTime)=>
      checkStartTime = endTime
      checking = false
      context.stop(sender)
      
    case DataCheck=>
      if(!dataChecking){
        dataChecking = true
        val worker = context.actorOf(Props[DataAlarmChecker], name = "dataChecker" + (Math.random()*1000).toInt)
        worker ! Start(dataCheckingTime)
      }
    case DataCheckFinish=>
      dataChecking = false
      context.stop(sender)
  }
}