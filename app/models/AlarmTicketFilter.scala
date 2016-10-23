package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._
import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.concurrent.Akka
import play.api.libs.json._
object AlarmTicketNotification extends Enumeration {
  val preticket = Value
  val repairing = Value
  val closePending = Value
  val map = Map(preticket -> "待立案", repairing -> "維修中案件", closePending -> "等待簽結案件")

  implicit val mReads: Reads[AlarmTicketNotification.Value] = EnumUtils.enumReads(AlarmTicketNotification)
  implicit val mWrites: Writes[AlarmTicketNotification.Value] = EnumUtils.enumWrites
}

object AlarmTicketFilter {
  case object Check
  case class Register(filter: Seq[AlarmTicketNotification.Value], receiver: ActorRef)
  case class Deregister(receiver: ActorRef)
  case class Notification(notification: AlarmTicketNotification.Value)

  var worker: ActorRef = _

  def start = {
    worker = Akka.system.actorOf(Props[AlarmTicketFilter], name = "AlarmTicketFilter")
  }

  def register(filter: Seq[AlarmTicketNotification.Value], receiver: ActorRef) = {
    worker ! Register(filter, receiver)
  }

  def deregister(receiver: ActorRef) = {
    worker ! Deregister(receiver)
  }
}

class AlarmTicketFilter extends Actor {
  import AlarmTicketFilter._
  var timer = Akka.system.scheduler.scheduleOnce(scala.concurrent.duration.Duration(5, scala.concurrent.duration.SECONDS), self, Check)
  var preticketReceivers = Seq.empty[ActorRef]
  var repairingReceivers = Seq.empty[ActorRef]
  var closePendingReceivers = Seq.empty[ActorRef]

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
    case Register(filter, receiver) =>
      filter map {
        _ match {
          case AlarmTicketNotification.preticket =>
            preticketReceivers = preticketReceivers :+ (receiver)
          case AlarmTicketNotification.repairing =>
            repairingReceivers = repairingReceivers :+ (receiver)
          case AlarmTicketNotification.closePending =>
            closePendingReceivers = closePendingReceivers :+ (receiver)
        }
      }
    case Deregister(receiver) =>
      preticketReceivers = preticketReceivers filter { r => r != receiver }
      repairingReceivers = repairingReceivers filter { r => r != receiver }
      closePendingReceivers = closePendingReceivers filter { r => r != receiver }

    case Notification(notification) =>
      notification match {
        case AlarmTicketNotification.preticket =>
          preticketReceivers map { _ ! Notification(notification) }
        case AlarmTicketNotification.repairing =>
          repairingReceivers map { _ ! Notification(notification) }
        case AlarmTicketNotification.closePending =>
          closePendingReceivers map { _ ! Notification(notification) }
      }
  }

  def alarmFilter(checkPoint: DateTime) = {
    val alarms = Alarm.getAlarmNoTicketList(checkPoint)
    for (ar <- alarms) {
      if (ar.code == MonitorStatus.REPAIR || ar.code == MonitorStatus.MAINTANCE_STAT || ar.code == "053")
        Alarm.updateAlarmTicketState(ar.monitor, ar.mItem, ar.time, "PAS")
      else {
        val countOpt = findSameAlarm(ar.monitor, ar.mItem, ar.code)(ar.time - 30.minute, ar.time)
        if (countOpt.isDefined && countOpt.get != 0) {
          Alarm.updateAlarmTicketState(ar.monitor, ar.mItem, ar.time, "PAS")
        }else{
          preticketReceivers map { _ ! Notification(AlarmTicketNotification.preticket) }
        }
      }
    }
    if (!alarms.isEmpty) {
      val lastAlarmTime = alarms.last.time
      context become handler(Some(lastAlarmTime))
    }

    timer = Akka.system.scheduler.scheduleOnce(scala.concurrent.duration.Duration(1, scala.concurrent.duration.MINUTES), self, Check)
  }

  override def postStop() {
    timer.cancel()
  }

}