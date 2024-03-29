package models
import com.github.nscala_time.time.Imports._
import scalikejdbc._
import ModelHelper._

case class EpaTicket(time:DateTime, monitor:EpaMonitor.Value,
                     monitorType: MonitorType.Value, value:Float, overStd:Int)

object EpaTicket {
  def create()(implicit session: DBSession = AutoSession){
    sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
          WHERE TABLE_NAME = 'EpaTickets'

          IF @@ROWCOUNT = 0
          BEGIN
            CREATE TABLE [dbo].[EpaTickets](
	          [M_DateTime] [datetime] NOT NULL,
	          [Monitor] [nvarchar](50) NOT NULL,
	          [MonitorType] [nvarchar](50) NOT NULL,
	          [Value] [float] NOT NULL,
	          [OverStd] [int] NOT NULL,
            CONSTRAINT [PK_EpaTickets] PRIMARY KEY CLUSTERED
            (
	            [M_DateTime] ASC,
	            [Monitor] ASC,
	            [MonitorType] ASC
            )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
            ) ON [PRIMARY]
          END
        """.execute().apply()
    val now = DateTime.now()

    regenerateEpaTickets(now - 15.day, now)
  }

  def mapper: WrappedResultSet => EpaTicket = {
    r=> EpaTicket(r.timestamp(1), EpaMonitor.withName(r.string(2)),
      MonitorType.withName(r.string(3)), r.float(4), r.int(5))
  }

  def getTickets(from :DateTime)(implicit session: DBSession = AutoSession): List[EpaTicket] = {
    val start: java.sql.Timestamp = from
    sql"""
         SELECT *
         From EpaTickets
         Where M_DateTime >= ${start}
         Order by M_DateTime Desc
         """.map(mapper).list().apply()
  }

  def upsert(ticket:EpaTicket, notify:Boolean = true)(implicit session: DBSession = AutoSession): Int = {
    val ticketTime: java.sql.Timestamp = ticket.time

    val ret1: Int =
      sql"""
         UPDATE [dbo].[EpaTickets]
          SET
              [Value] = ${ticket.value}
              ,[OverStd] = ${ticket.overStd}
          WHERE [M_DateTime]= ${ticketTime} and [Monitor] = ${ticket.monitor.toString} and
            [MonitorType] = ${ticket.monitorType.toString}
         """.update().apply()

    if(ret1 != 0)
      ret1
    else
    {
      if(notify)
        LineNotify.notifyEpaGroup(
        s"${ticket.time.toString("yyyy/MM/dd HH:mm")}-${EpaMonitor.map(ticket.monitor).name}${MonitorType.map(ticket.monitorType).desp}超過內控值")
      sql"""
            INSERT INTO [dbo].[EpaTickets]
            ([M_DateTime]
            ,[Monitor]
            ,[MonitorType]
            ,[Value]
            ,[OverStd])
            VALUES
            (${ticketTime}
              ,${ticket.monitor.toString}
              ,${ticket.monitorType.toString}
              ,${ticket.value}
              ,${ticket.overStd})
         """.update().apply()
    }



  }

  def regenerateEpaTickets(start:DateTime, end:DateTime)(implicit session:DBSession = AutoSession): Unit = {
    for{epaMonitor<- EpaMonitor.epaList
        mt <- MonitorType.eapIdMap.values}{
      val overStdCode: Int = AlarmLevel.map(AlarmLevel.Internal).code
      val epaRecords = Record.getEpaHourRecord(epaMonitor, mt, start, end)
      epaRecords.foreach(record=>{
        if(EpaMonitorTypeAlert.map.contains(epaMonitor) && EpaMonitorTypeAlert.map(epaMonitor).contains(mt))
          for (internal <- EpaMonitorTypeAlert.map(epaMonitor)(mt).internal if record.value >= internal) {
            EpaTicket.upsert(EpaTicket(record.time, epaMonitor, mt, record.value, overStdCode), false)
          }
      })
    }
  }

}
