package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
import models.Realtime._
import ModelHelper._

case object AlarmCheck
case object DataCheck
case object MaintanceTicketCheck

class AlarmMaster extends Actor{
  var checkStartTime = DateTime.now - 10.minute
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
        
    case MaintanceTicketCheck =>
        val activeMaintanceTickets = Ticket.queryActiveMaintanceTickets(DateTime.yesterday().withMillisOfDay(0))
        for(ticket <- activeMaintanceTickets){
          val ar = Alarm.Alarm(ticket.monitor, ticket.ticketType.toString(), DateTime.now, 1, "038")
          Alarm.insertAlarm(ar)
        }
    case DataCheckFinish=>
      sender!PoisonPill
  }
}