import play.api._
import scalikejdbc._
import scalikejdbc.config._
import models._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Global extends GlobalSettings {  
  override def onStart(app: Application) {
    Logger.info("Application has started")
    //DBs.setupAll()
    super.onStart(app)
    val alarmActor = Akka.system.actorOf(Props[AlarmMaster], name = "AlarmMaster")
    
    Akka.system.scheduler.schedule(Duration(5, SECONDS), Duration(60, SECONDS), alarmActor, StartCheck)
  }

  override def onStop(app: Application) {
    Logger.info("Application shutdown...")
    //DBs.closeAll()
    super.onStop(app)
  }
}