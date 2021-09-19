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
  def upgradeDB() {
    DB autoCommit {
      implicit session =>
        sql"""
             IF COL_LENGTH('[dbo].[MonitorType]', 'Warn') IS NULL
             BEGIN
                 ALTER TABLE [dbo].[MonitorType] ADD Warn Decimal(9,2) NULL,
                 EightHrAvg Decimal(9,2) NULL, DayAvg Decimal(9,2) Null;
             END
             """.execute().apply()
    }

    DB autoCommit( {
      implicit session =>
        sql"""
              SELECT TABLE_NAME
              FROM INFORMATION_SCHEMA.TABLES
              WHERE TABLE_NAME = 'MonitorTypeOverride'

              IF @@ROWCOUNT = 0
              BEGIN
                CREATE TABLE [dbo].[MonitorTypeOverride](
	              [DP_NO] [varchar](6) NOT NULL,
	              [ITEM] [varchar](4) NOT NULL,
	              [STD_Internal] [decimal](9, 2) NULL,
	              [STD_Law] [decimal](9, 2) NULL,
	              [STD_Hour] [decimal](9, 2) NULL,
	              [STD_Day] [decimal](9, 2) NULL,
	              [STD_Year] [decimal](9, 2) NULL,
	              [Warn] [decimal](9, 2) NULL,
	              [EightHrAvg] [decimal](9, 2) NULL,
	              [DayAvg] [decimal](9, 2) NULL,
	              [YearAvg] [decimal](9, 2) NULL,
	              [precision] [decimal](9, 2) NULL,
                CONSTRAINT [PK_MonitorTypeOverride] PRIMARY KEY CLUSTERED
                  (
	                  [DP_NO] ASC,
	                  [ITEM] ASC
                  )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
                  ) ON [PRIMARY]
                END
             """.execute().apply()
    })
  }

  override def onStart(app: Application) {
    Logger.info("Application has started")
    DBs.setupAll()
    super.onStart(app)
    upgradeDB()

    val alarmActor = Akka.system.actorOf(Props[AlarmMaster], name = "AlarmMaster")
    
    Akka.system.scheduler.schedule(Duration(3, MINUTES), Duration(5, MINUTES), alarmActor, AlarmCheck)
    Akka.system.scheduler.schedule(Duration(3, MINUTES), Duration(10, MINUTES), alarmActor, DataCheck)
    Akka.system.scheduler.schedule(Duration(secondToTomorror1AM, SECONDS), Duration(1, DAYS), alarmActor, MaintanceTicketCheck)
    
    AlarmTicketFilter.start
    PartAlarmWorker.start
    DbUpdater.start
  }

  def secondToTomorror1AM = {
    import com.github.nscala_time.time.Imports._
    val tomorrow_1AM = DateTime.tomorrow().withMillisOfDay(0).withHourOfDay(1)
    val duration = new Duration(DateTime.now(), tomorrow_1AM)
    duration.getStandardSeconds    
  }
  
  override def onStop(app: Application) {
    Logger.info("Application shutdown...")
    super.onStop(app)
  }
}