package models

import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models.Record.{HourRecord, getHourRecords}
import org.joda.time.format.DateTimeFormat
import play.api.Logger
import scalikejdbc._

case class AggregateReport2(time: DateTime, monitor: Monitor.Value, monitorType: MonitorType.Value, value: Float,
                            stdLaw: Option[Float], windDir: Option[Float], windAngle: String, windSpeed: Option[Float],
                            action: String, state: String)


object AggregateReport2 {
  val stateList= Seq("待確認", "儀器異常", "儀器無異常")

  val dirMap =
    Map(
      (0 -> "北"), (1 -> "北北東"), (2 -> "東北"), (3 -> "東北東"), (4 -> "東"),
      (5 -> "東南東"), (6 -> "東南"), (7 -> "南南東"), (8 -> "南"),
      (9 -> "南南西"), (10 -> "西南"), (11 -> "西西南"), (12 -> "西"),
      (13 -> "西北西"), (14 -> "西北"), (15 -> "北北西"))

  def createTab(implicit session: DBSession = AutoSession) = {
    DateTime.now.toString("yyyy/M/d HH:mm")
    sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
          WHERE TABLE_NAME = 'AggregateReport2'

          IF @@ROWCOUNT = 0
          BEGIN
            CREATE TABLE [dbo].[AggregateReport2](
	            [time] [datetime] NOT NULL,
	            [monitor] [nvarchar](10) NOT NULL,
	            [monitorType] [nvarchar](10) NOT NULL,
	            [value] [float] NOT NULL,
	            [StdLaw] [float] NULL,
	            [WindDir] [float] NULL,
	            [WindAngle] [nvarchar](10) NOT NULL,
	            [WindSpeed] [float] NULL,
	            [Action] [nvarchar](50) NOT NULL,
	            [state] [nvarchar](50) NOT NULL,
              CONSTRAINT [PK_AggregateReport2] PRIMARY KEY CLUSTERED
            (
	            [time] ASC,
	            [monitor] ASC,
	            [monitorType] ASC
            )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
            ) ON [PRIMARY]
          END
         """.execute().apply()
  }

  def updateAction(time:DateTime, monitor:Monitor.Value, monitorType: MonitorType.Value,
                   action:String, state:String)(implicit session: DBSession = AutoSession) = {

      sql"""
        UPDATE [dbo].[AggregateReport2]
          SET
              [Action] = ${action},
              [State] = ${state}
          WHERE [time] = ${time.toDate} and [monitorType] = ${monitorType.toString} and monitor = ${monitor.toString}
         """.update().apply()

  }

  def updateState(monitor:Monitor.Value, monitorType: MonitorType.Value, start:DateTime, end:DateTime, state:String)
                 (implicit session: DBSession = AutoSession) = {
    sql"""
        UPDATE [dbo].[AggregateReport2]
          SET
              [State] = ${state}
          WHERE [time] >= ${start.toDate} and [time] < ${end.toDate} and [monitorType] = ${monitorType.toString} and monitor = ${monitor.toString}
         """.update().apply()
  }

  def upsert(reportList: Seq[AggregateReport2])(implicit session: DBSession = AutoSession) = {
    for (report <- reportList) {
      val timeT: java.sql.Timestamp = report.time

      sql"""
        UPDATE [dbo].[AggregateReport2]
        SET
            [value] = ${report.value}
            ,[StdLaw] = ${report.stdLaw}
            ,[WindDir] = ${report.windDir}
            ,[WindAngle] = ${report.windAngle}
            ,[WindSpeed] = ${report.windSpeed}
          WHERE [time] = ${timeT} and [monitor] = ${report.monitor.toString} and [monitorType] = ${report.monitorType.toString}
          IF(@@ROWCOUNT = 0)
          BEGIN
          INSERT INTO [dbo].[AggregateReport2]
           ([time]
           ,[monitor]
           ,[monitorType]
           ,[value]
           ,[StdLaw]
           ,[WindDir]
           ,[WindAngle]
           ,[WindSpeed]
           ,[Action]
           ,[state])
     VALUES
           (${timeT}
           ,${report.monitor.toString}
           ,${report.monitorType.toString}
           ,${report.value}
           ,${report.stdLaw}
           ,${report.windDir}
           ,${report.windAngle}
           ,${report.windSpeed}
           ,${report.action}
           ,${report.state})
          END
         """.update().apply()
    }
  }

  def query(monitors: Seq[Monitor.Value],
            monitorTypes: Seq[MonitorType.Value],
            start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val startT: java.sql.Timestamp = start
    val endT: java.sql.Timestamp = end
    val monitorSeq = SQLSyntax.createUnsafely(monitors.map(_.toString).mkString("('", "','", "')"))
    val monitorTypeSeq = SQLSyntax.createUnsafely(monitorTypes.map(_.toString).mkString("('", "','", "')"))
    sql"""
           Select *
           From AggregateReport2
           Where time between ${startT} and ${endT} and monitor in $monitorSeq and monitorType in $monitorTypeSeq
           """.map(rs => AggregateReport2(time = rs.timestamp(1), monitor = Monitor.withName(rs.string(2)),
      monitorType = MonitorType.withName(rs.string(3)), value = rs.float(4),
      stdLaw = rs.floatOpt(5), windDir = rs.floatOpt(6), windAngle = rs.string(7),
      windSpeed = rs.floatOpt(8),
      action = rs.string(9), state = rs.string(10))).list().apply()
  }

  def generate(m: Monitor.Value, recordList: List[HourRecord]): Unit = {
    val mCase = Monitor.map(m)

    val reportOptList: Seq[Option[AggregateReport2]] =
      for {mt <- mCase.monitorTypes
           records = recordList.map(h =>
             (Record.timeProjection(h).toJodaDateTime,
               Record.monitorTypeProject2(mt)(h), Record.monitorTypeProject2(MonitorType.C211)(h),
               Record.monitorTypeProject2(MonitorType.C212)(h)))
           (time, (valueOpt, statusOpt), (windSpeedOpt, _), (windDirOpt, _)) <- records
           value <- valueOpt
           status <- statusOpt if MonitorStatus.isNormalStat(status)
           stdLawOpt = MonitorTypeAlert.map(m)(mt).std_law
           internal <-MonitorTypeAlert.map(m)(mt).internal
           } yield {
        val dirDesc =
          for(windDir <- windDirOpt)yield
            dirMap(Math.ceil((windDir - 22.5 / 2) / 22.5).toInt % 16)

        if((value >= internal) || (stdLawOpt.isDefined && value >= stdLawOpt.get)) {
          Some(AggregateReport2(time, m, mt, value, stdLawOpt, windDirOpt, dirDesc.getOrElse(""), windSpeedOpt, "", "待確認"))
        }else
          None
      }
    val reportList = reportOptList.flatten
    upsert(reportList)
  }

  def generatePast(): Unit ={
    val start = DateTime.parse("2021-01-01", DateTimeFormat.forPattern("yyyy-MM-dd"))
    for(m<-Monitor.mvList){
      val recordList = Record.getHourRecords(m, start, DateTime.now)
      generate(m, recordList)
    }
  }
}
