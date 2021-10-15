package models
import com.github.nscala_time.time.Imports._
import scalikejdbc._
import ModelHelper._

case class EpaTicket(time:DateTime, monitor:EpaMonitor.Value,
                     monitorType: MonitorType.Value, value:Float, overStd:Int)

object EpaTicket {
  def create()(implicit session: DBSession = AutoSession): Boolean ={
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
  }

  def mapper: WrappedResultSet => EpaTicket = {
    r=> EpaTicket(r.date(1), EpaMonitor.withName(r.string(2)),
      MonitorType.withName(r.string(3)), r.float(4), r.int(5))
  }

  def getTickets(from :DateTime)(implicit session: DBSession = AutoSession) = {
    val start: java.sql.Timestamp = from
    sql"""
         SELECT *
         From EpaTickets
         Where M_DateTime >= ${start}
         Order by M_DateTime Desc
         """.map(mapper).list().apply()
  }

  def upsert(ticket:EpaTicket)(implicit session: DBSession = AutoSession) = {
    sql"""
         UPDATE [dbo].[EpaTickets]
          SET
              [Value] = ${ticket.value}
              ,[OverStd] = ${ticket.overStd}
          WHERE [M_DateTime]= ${ticket.time.toDate} and [Monitor] = ${ticket.monitor.toString} and
            [MonitorType] = ${ticket.monitorType.toString}
         IF(@@ROWCOUNT = 0)
          BEGIN
            INSERT INTO [dbo].[EpaTickets]
            ([M_DateTime]
            ,[Monitor]
            ,[MonitorType]
            ,[Value]
            ,[OverStd])
            VALUES
            (${ticket.time.toDate}
              ,${ticket.monitor.toString}
              ,${ticket.monitorType.toString}
              ,${ticket.value}
              ,${ticket.overStd})
          END
         """.update().apply()
  }

}
