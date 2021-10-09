import akka.actor._
import models._
import play.api.Play.current
import play.api._
import play.api.libs.concurrent.Akka
import scalikejdbc._
import scalikejdbc.config._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Global extends GlobalSettings {
  def upgradeEpaDataTab()(implicit session: DBSession): Unit = {
    sql"""
              IF COL_LENGTH('[dbo].[Hour_data]', 'ID') IS NOT NULL
                Begin
                  DROP TABLE hour_data;
                  CREATE TABLE [dbo].[hour_data](
	                  [MStation] [int] NOT NULL,
	                  [MDate] [datetime2](7) NOT NULL,
	                  [MItem] [varchar](50) NOT NULL,
	                  [MValue] [float] NOT NULL,
                    CONSTRAINT [PK_hour_data] PRIMARY KEY CLUSTERED
                    (
	                    [MStation] ASC,
	                    [MDate] ASC,
	                    [MItem] ASC
                    )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
                  ) ON [PRIMARY];
                End
             """.execute().apply()
  }

  def upgradeDB() {
    DB autoCommit {
      implicit session =>
        def alterIfMissing(field: String) = {
          val fieldName = SQLSyntax.createUnsafely(s"$field")
          sql"""
             IF COL_LENGTH('[dbo].[MonitorType]', '$fieldName') IS NULL
             BEGIN
                 ALTER TABLE [dbo].[MonitorType] ADD $fieldName Decimal(9,2) NULL;
             END
             """.execute().apply()
        }

        import com.github.nscala_time.time.Imports._
        //Create this year Ozone 8hr table
        val thisYear = DateTime.now().getYear
        if (!Ozone8Hr.hasOzone8hrTab(thisYear))
          Ozone8Hr.createTab(thisYear)

        upgradeEpaDataTab()
        if(!MonitorTypeAlert.hasTable()){
          MonitorTypeAlert.createMonitorTypeAlert()
          MonitorTypeAlert.initTable()
        }
        if(!EpaMonitorTypeAlert.hasTable()){
          EpaMonitorTypeAlert.createMonitorTypeAlert()
          EpaMonitorTypeAlert.initTable()
        }
        Ticket.upgrade()
        EpaTicket.create()
    }
    OverStdConverter.start
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

    MonitorTypeAlert.init()
    EpaMonitorTypeAlert.init()
    AlarmTicketFilter.start
    PartAlarmWorker.start
    DbUpdater.start
    OpenDataReceiver.startup()
    Ozone8HrCalculator.start
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