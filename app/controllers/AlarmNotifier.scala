package controllers
import scalikejdbc._
import scalikejdbc.config._
import akka.actor._
import play.api._
import play.api.mvc._ 
import com.github.nscala_time.time.Imports._
import models._
import play.api.libs.concurrent.Akka
import akka.actor._
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._


object AlarmNotifier {
  def props(out: ActorRef) = Props(new AlarmNotifier(out))
}

class AlarmNotifier(out: ActorRef) extends Actor {
  var lastCheckTime = DateTime.now  

  object CmdType extends Enumeration{
    val start = Value /* start:userId */
    val alert = Value /* */
  }

  val cancelable = Akka.system.scheduler.schedule(scala.concurrent.duration.Duration(10, SECONDS),
          scala.concurrent.duration.Duration(1, MINUTES), self, AlarmCheck)

  var progress: Int = _
  var userId:Int = _
  def parseStartCmd(msg:String)={
    val param = msg.split(":")
    (CmdType.withName(param(0)), param(1).toInt)
  }

  def receive = {
    case msg: String =>
      val (cmd, id) = parseStartCmd(msg)
      userId = id
      
    case AlarmCheck =>
      checkAlarm   
  }

  override def postStop(): Unit = {
    cancelable.cancel()
  }
  
  def checkAlarm() {
    val userOpt = User.getUserById(userId)
    if (userOpt.isEmpty)
      return

    val user = userOpt.get
    if (user.alarmConfig.isEmpty)
      return

    val alarmConfig = user.alarmConfig.get

    if (alarmConfig.enable) {
      val alarms = Alarm.getAlarm(Monitor.mvList, Some(MonitorStatus.alarmList), lastCheckTime, DateTime.now)
      for (ar <- alarms) {
        if (alarmConfig.monitorFilter.contains(ar.monitor) &&
          alarmConfig.statusFilter.contains(ar.code)) {
          val ar_state = 
            if(ar.mVal == 0)
              "恢復正常"
            else
              "觸發"
              
          val msg = s"${CmdType.alert}!${ar.time.toString("MM-dd HH:mm")} ${Monitor.map(ar.monitor).name}:${Alarm.map(ar.mItem)}-${MonitorStatus.map(ar.code).desp}:${ar_state}"
          out ! msg
        }
      }

      if (alarms.length != 0) {
        val latestTime = alarms.last.time
        lastCheckTime = latestTime.plusSeconds(1)

      }
    }
  }
}