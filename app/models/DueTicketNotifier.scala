package models

import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import play.api.libs.concurrent.Akka

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object DueTicketNotifier{
  def props(out: ActorRef) = Props(classOf[DueTicketNotifier], out)
  case object DueTicketCheck
}

class DueTicketNotifier(out: ActorRef) extends Actor {
  var lastCheckTime = DateTime.now
  import DueTicketNotifier._

  object CmdType extends Enumeration {
    val start = Value /* start:userId */
    val alert = Value /* */
    val notification = Value /*notification:[preticket/repairing/closePending] */
  }

  val cancelable = Akka.system.scheduler.schedule(scala.concurrent.duration.Duration(10, SECONDS),
    scala.concurrent.duration.Duration(1, scala.concurrent.duration.HOURS), self, DueTicketCheck)

  var progress: Int = _
  var userId: Int = _
  def parseStartCmd(msg: String) = {
    val param = msg.split(":")
    (CmdType.withName(param(0)), param(1).toInt)
  }

  def receive = {
    case msg: String =>
      val (cmd, id) = parseStartCmd(msg)
      userId = id

    case DueTicketCheck =>
      checkDueTicket
  }

  override def postStop(): Unit = {
    cancelable.cancel()
  }

  def checkDueTicket() {
    val dueTickets = Ticket.getActiveRepairDueTicketsByGroup
    val msg = s"${CmdType.alert}!${dueTickets.size}:已經有${dueTickets.size}逾期!"
    out ! msg
  }
}