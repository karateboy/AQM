import play.api._
import scalikejdbc._
import scalikejdbc.config._
import models._

object Global extends GlobalSettings {  
  override def onStart(app: Application) {
    Logger.info("Application has started")
    MonitoredType.init
    //DBs.setupAll()
    super.onStart(app)
  }

  override def onStop(app: Application) {
    Logger.info("Application shutdown...")
    //DBs.closeAll()
    super.onStart(app)
  }
}