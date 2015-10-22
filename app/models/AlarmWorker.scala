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
    val alarms = Alarm.getAlarm(Monitor.mvList, Some(MonitorStatus.alarmNotificationList), startTime, DateTime.now)
    if(alarms.length == 0)
      startTime
    else{
      val adminUserList = User.getAdminUsers()
      for(user <- adminUserList){
        if(user.alarmConfig.isDefined){
          val alarmConfig = user.alarmConfig.get
          if(alarmConfig.enable){
            val matchedAlarms = alarms.filter { alarm => alarmConfig.monitorFilter.contains(alarm.monitor) && 
              alarmConfig.statusFilter.contains(alarm.code) }
            for(ar <- matchedAlarms){
              sendAlarmEmail(user, ar)
            }
          }
        }
      }
      val latestTime = alarms.last.time
      latestTime + 1.second
    }
  }
  
  import play.api.libs.mailer._
  def sendAlarmEmail(user:User, alarm: Alarm)={
    Logger.debug(s"send alarm to: ${user.name} - ${alarm.time.toString}:${Alarm.map(alarm.mItem)}:${MonitorStatus.map(alarm.code).desp}")
    val email = Email(
      s"警報: ${Monitor.map(alarm.monitor).name}-${Alarm.map(alarm.mItem)}(${MonitorStatus.map(alarm.code).desp})",
      "警報服務 <karateboy.huang@gmail.com>",
      Seq(user.email),
      // adds attachment
      attachments = Seq(),
      // sends text, HTML or both...
      bodyText = Some(s"${Monitor.map(alarm.monitor).name}- ${alarm.time.toString}:${Alarm.map(alarm.mItem)}:${MonitorStatus.map(alarm.code).desp}"),
      bodyHtml = Some(s"<html><body><p><b>${Monitor.map(alarm.monitor).name}-${alarm.time.toString("YYYY/MM/dd HH:mm")}:${Alarm.map(alarm.mItem)}:${MonitorStatus.map(alarm.code).desp}</b></p></body></html>")
    )
    
    MailerPlugin.send(email)
  }
}