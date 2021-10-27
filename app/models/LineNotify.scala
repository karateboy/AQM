package models

import models.Alarm.Alarm
import play.api.Logger
import play.api.Play.current
import play.api.libs.ws.WS

import scala.concurrent.ExecutionContext.Implicits.global
object LineNotify {
  def notify(msg:String)={
    val f = WS.url("https://notify-api.line.me/api/notify").
      withHeaders("Authorization"-> s"Bearer Xdv0biY8gzKOhIsUdtNiEOhJVSPmnXiNRQfSLN8zFpa",
        "Content-Type"->"application/x-www-form-urlencoded")
      .post(Map("message" -> Seq(msg)))

    for(ret<-f) yield {
      if(ret.status != 200)
        Logger.error(ret.body)
    }
  }

  def notifyAlarm(alarm:Alarm): Unit ={
    val ar_state =
      if (alarm.mVal == 0)
        "恢復正常"
      else
        "觸發"

    val msg = s"${Monitor.map(alarm.monitor).name}-${alarm.time.toString("MM/dd HH:mm")}:${Alarm.getItem(alarm)}:${Alarm.getReason(alarm)}:${ar_state}"
    notify(msg)
  }

  def notifyTicket(ticket:Ticket): Unit ={
    val ticketType = TicketType.map(ticket.ticketType)
    val monitor = Monitor.map(ticket.monitor).name
    val monitorType = ticket.monitorType.map(mt=>MonitorType.map(mt).desp).getOrElse("")

    val msg = s"新增案件: ${ticketType}-${monitor}${monitorType}-${ticket.reason}"
    notify(msg)
  }
}
