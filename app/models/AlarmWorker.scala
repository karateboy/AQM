package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import Alarm._

sealed trait AlarmCheckMsg
case class Start(startTime: DateTime) extends AlarmCheckMsg
case class Finish(latestTime: DateTime) extends AlarmCheckMsg

class AlarmWorker extends Actor{
  def receive = {
    case Start(startTime)=>
      sender ! Finish(checkAlarm(startTime)) // perform the work
  }
  
  def checkAlarm(startTime: DateTime)={
    val alarms = Alarm.getAlarm(Monitor.mvList, Some(MonitorStatus.alarmList), startTime, DateTime.now)
    if(alarms.length == 0)
      startTime
    else{
      val adminUserList = User.getAdminUsers()
      for (ar <- alarms) {
        val matchedUser = adminUserList.filter { user =>
          user.alarmConfig.isDefined && {
            val alarmConfig = user.alarmConfig.get
            alarmConfig.enable && alarmConfig.monitorFilter.contains(ar.monitor) &&
              alarmConfig.statusFilter.contains(ar.code)
          }
        }
        if (matchedUser.length != 0){
          val userName = matchedUser.map { _.name}.mkString(",")
          try {
            sendAlarmEmail(matchedUser, ar)            
            EventLog.create(EventLog(DateTime.now, EventLog.evtTypeInformAlarm,
              s"送信警告信給${userName} ${Monitor.map(ar.monitor).name}-${Alarm.map(ar.mItem)}-${MonitorStatus.map(ar.code).desp}"))
          } catch {
            case ex: Exception =>
              Console.print(ex.toString)
              EventLog.create(EventLog(DateTime.now, EventLog.evtTypeInformAlarm, s"無法送信警告信給${userName}:${ex.getCause}"))
          }
        }
      }

      val latestTime = alarms.last.time
      latestTime + 1.second
    }
  }
  
  import play.api.libs.mailer._
  def sendAlarmEmail(users: List[User], alarm: Alarm) = {
    val ar_state =
      if (alarm.mVal == 0)
        "恢復正常"
      else
        "觸發"

    val msg = s"${Monitor.map(alarm.monitor).name}- ${alarm.time.toString}:${Alarm.map(alarm.mItem)}:${MonitorStatus.map(alarm.code).desp}:${ar_state}"
    val htmlMsg = s"<html><body><p><b>${Monitor.map(alarm.monitor).name}-${alarm.time.toString("YYYY/MM/dd HH:mm")}:${Alarm.map(alarm.mItem)}:${MonitorStatus.map(alarm.code).desp}:${ar_state}</b></p></body></html>"
    val email = Email(
      s"警報: ${Monitor.map(alarm.monitor).name}-${Alarm.map(alarm.mItem)}(${MonitorStatus.map(alarm.code).desp})",
      "警報服務 <karateboy.huang@gmail.com>",
      users.map { _.email },
      // adds attachment
      attachments = Seq(),
      // sends text, HTML or both...
      bodyText = Some(msg),
      bodyHtml = Some(htmlMsg))

    SmsSender.send(users, msg)
    MailerPlugin.send(email)
  }
}