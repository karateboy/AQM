package models

import scalikejdbc._

case class MonitorTypeAlert(monitor: Monitor.Value, monitorType: MonitorType.Value,
                            internal: Option[Float], warn: Option[Float], std_law: Option[Float],
                            std_hour: Option[Float], std_day: Option[Float],
                            eightHrAvg: Option[Float], twentyFourHrAvg: Option[Float],
                            yearAvg: Option[Float]){
  def getInfo: MonitorTypeAlertInfo = {
    val mCase = Monitor.map(monitor)
    val mtCase = MonitorType.map(monitorType)
    MonitorTypeAlertInfo(mCase.id, mtCase.id, mtCase.desp, mtCase.unit, mtCase.prec,
      internal, warn, std_law, std_hour, std_day, eightHrAvg, twentyFourHrAvg, yearAvg)
  }
}

case class MonitorTypeAlertInfo(monitorID: String, monitorTypeID: String, name:String, unit:String, prec: Int,
                                internal: Option[Float], warn: Option[Float], std_law: Option[Float],
                                std_hour: Option[Float], std_day: Option[Float],
                                eightHrAvg: Option[Float], twentyFourHrAvg: Option[Float],
                                yearAvg: Option[Float])

object MonitorTypeAlert {
  def hasTable()(implicit session: DBSession) = {
    val list = {
      sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
         """.map { rs => rs.string(1) }.list().apply()
    }
    list.contains("MonitorTypeAlert")
  }

  def createMonitorTypeAlert()(implicit session: DBSession): Unit = {
    sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
          WHERE TABLE_NAME = 'MonitorTypeAlert'

          IF @@ROWCOUNT = 0
          BEGIN
                CREATE TABLE [dbo].[MonitorTypeAlert](
	              [DP_NO] [varchar](6) NOT NULL,
	              [ITEM] [varchar](4) NOT NULL,
	              [STD_Internal] [decimal](9, 2) NULL,
                [Warn] [decimal](9, 2) NULL,
	              [STD_Law] [decimal](9, 2) NULL,
	              [STD_Hour] [decimal](9, 2) NULL,
	              [STD_Day] [decimal](9, 2) NULL,
                [EightHrAvg] [decimal](9, 2) NULL,
                [TwentyFourHrAvg] [decimal](9, 2) NULL,
	              [STD_Year] [decimal](9, 2) NULL,
                CONSTRAINT [PK_MonitorTypeAlert] PRIMARY KEY CLUSTERED
                  (
	                  [DP_NO] ASC,
	                  [ITEM] ASC
                  )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
                  ) ON [PRIMARY]
                END
             """.execute().apply()
  }

  def initTable()(implicit session: DBSession): Unit = {
    val mtaList =
      for {m <- Monitor.mvList
           mt <- MonitorType.mtvList
           mCase = Monitor.map(m)
           mtCase = MonitorType.map(mt)
           } yield {
        MonitorTypeAlert(m, mt, mCase.getStdInternal(mt), None, mtCase.std_law, mtCase.std_hour, mtCase.std_day,
          None, None, None)
      }

    val paramList:Seq[Seq[Any]] = mtaList map { mta =>
      Seq(
        mta.monitor.toString
        , mta.monitorType.toString
        , mta.internal
        , mta.warn
        , mta.std_law
        , mta.std_hour
        , mta.std_day
        , mta.eightHrAvg
        , mta.twentyFourHrAvg
        , mta.yearAvg
      )
    }

    sql"""
        INSERT INTO [dbo].[MonitorTypeAlert]
           ([DP_NO]
           ,[ITEM]
           ,[STD_Internal]
           ,[Warn]
           ,[STD_Law]
           ,[STD_Hour]
           ,[STD_Day]
           ,[EightHrAvg]
           ,[TwentyFourHrAvg]
           ,[STD_Year])
     VALUES
           (?
           ,?
           ,?
           ,?
           ,?
           ,?
           ,?
           ,?
           ,?
           ,?)
         """.batch(paramList:_*).apply()
  }

  def save(mta: MonitorTypeAlert)(implicit session: DBSession) = {
    sql"""
        INSERT INTO [dbo].[MonitorTypeAlert]
           ([DP_NO]
           ,[ITEM]
           ,[STD_Internal]
           ,[Warn]
           ,[STD_Law]
           ,[STD_Hour]
           ,[STD_Day]
           ,[EightHrAvg]
           ,[TwentyFourHrAvg]
           ,[STD_Year])
     VALUES
           (${mta.monitor.toString}
           ,${mta.monitorType.toString}
           ,${mta.internal}
           ,${mta.warn}
           ,${mta.std_law}
           ,${mta.std_hour}
           ,${mta.std_day}
           ,${mta.eightHrAvg}
           ,${mta.twentyFourHrAvg}
           ,${mta.yearAvg})
         """.update().apply()
  }

  var map = Map.empty[Monitor.Value, Map[MonitorType.Value, MonitorTypeAlert]]

  def init() = {
    DB readOnly {
      implicit session =>
        sql"""
               Select *
               From MonitorTypeAlert
               """.map(rs => {
          val m = Monitor.withName(rs.string(1))
          val mt = MonitorType.withName(rs.string(2))
          val mtMap: Map[MonitorType.Value, MonitorTypeAlert] = map.getOrElse(m, {
            map = map + (m -> Map.empty[MonitorType.Value, MonitorTypeAlert])
            Map.empty[MonitorType.Value, MonitorTypeAlert]
          })

          val pair = mt ->
            MonitorTypeAlert(m, mt, rs.floatOpt(3),
              rs.floatOpt(4),
              rs.floatOpt(5),
              rs.floatOpt(6),
              rs.floatOpt(7),
              rs.floatOpt(8),
              rs.floatOpt(9),
              rs.floatOpt(10)
            )
          val newMtMap: Map[MonitorType.Value, MonitorTypeAlert] = mtMap + pair
          map = map + (m -> newMtMap)
        }
        ).list().apply()

    }
  }
}
