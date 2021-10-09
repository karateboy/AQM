package models

import play.api.Logger
import scalikejdbc._

case class EpaMonitorTypeAlert(id: Int, monitorType: MonitorType.Value,
                               internal: Option[Float])

object EpaMonitorTypeAlert {
  def hasTable()(implicit session: DBSession) = {
    val list = {
      sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
         """.map { rs => rs.string(1) }.list().apply()
    }
    list.contains("EpaMonitorTypeAlert")
  }
  def createMonitorTypeAlert()(implicit session: DBSession): Unit = {
    sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
          WHERE TABLE_NAME = 'EpaMonitorTypeAlert'

          IF @@ROWCOUNT = 0
          BEGIN
                CREATE TABLE [dbo].[EpaMonitorTypeAlert](
	              [DP_NO] [int] NOT NULL,
	              [ITEM] [varchar](4) NOT NULL,
	              [STD_Internal] [decimal](9, 2) NULL,
                CONSTRAINT [PK_EpaMonitorTypeAlert] PRIMARY KEY CLUSTERED
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
      for {m <- EpaMonitor.epaList
           epaCase = EpaMonitor.map(m)
           mt <- MonitorType.mtvList if MonitorType.map(mt).epa_mapping.isDefined
           } yield {

        EpaMonitorTypeAlert(epaCase.id, mt, None)
      }
    Logger.info(s"Epa list #=${mtaList.size}")
    val paramList:Seq[Seq[Any]] = mtaList map { mta =>
      Seq(
          mta.id
        , mta.monitorType.toString
        , mta.internal
      )
    }
    sql"""
        INSERT INTO [dbo].[EpaMonitorTypeAlert]
           ([DP_NO]
           ,[ITEM]
           ,[STD_Internal]
           )
     VALUES
           (?
           ,?
           ,?
           )
         """.batch(paramList:_*).apply()
  }
  var map = Map.empty[EpaMonitor.Value, Map[MonitorType.Value, EpaMonitorTypeAlert]]

  def init() = {
    DB readOnly {
      implicit session =>
        sql"""
               Select *
               From EpaMonitorTypeAlert
               """.map(rs => {
          val m = EpaMonitor.idMap(rs.int(1))
          val mt = MonitorType.withName(rs.string(2))
          val mtMap: Map[MonitorType.Value, EpaMonitorTypeAlert] = map.getOrElse(m, {
            map = map + (m -> Map.empty[MonitorType.Value, EpaMonitorTypeAlert])
            Map.empty[MonitorType.Value, EpaMonitorTypeAlert]
          })

          val pair = mt ->
            EpaMonitorTypeAlert(rs.int(1), mt, rs.floatOpt(3)
            )
          val newMtMap: Map[MonitorType.Value, EpaMonitorTypeAlert] = mtMap + pair
          map = map + (m -> newMtMap)
        }
        ).list().apply()

    }
  }

  def save(mta: EpaMonitorTypeAlert)(implicit session: DBSession) = {
    sql"""
     UPDATE [dbo].[EpaMonitorTypeAlert]
        SET
         [STD_Internal] = ${mta.internal}
        WHERE [DP_NO] = ${mta.id} and [ITEM] = ${mta.monitorType.toString}
         """.update().apply()
    val m = EpaMonitor.idMap(mta.id)
    val mtMap = map(m)
    val newMtMap = mtMap + (mta.monitorType-> mta)
    map = map + (m-> newMtMap)
  }

}
