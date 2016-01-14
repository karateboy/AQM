package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._

case class ManualAuditLog(tabType:TableType.Value, monitor:Monitor.Value, dataTime:DateTime, monitorType:MonitorType.Value, 
    modified_time:DateTime, changed_status:String, operator:String, reason:Option[String])
object ManualAuditLog {
  val mapping = List(
      1-> TableType.SixSec,
      2-> TableType.Min,
      3 -> TableType.Hour
      )
      
  val tabIdxMap = Map(mapping:_*)
  val tabTypeToIdMap = Map(mapping.map(t=>t.swap):_*)
  
  def getLog(tabType:TableType.Value, monitor:Monitor.Value, dataTime:Timestamp, monitorType:MonitorType.Value)={
    DB readOnly{
      implicit session =>
        sql"""
          Select *
          From manualAuditLog
          Where tabType = ${tabTypeToIdMap(tabType)} and monitor=${monitor.toString()} and dataTime=${dataTime} and monitorType=${monitorType.toString}
          """.map { 
          rs => ManualAuditLog(tabType=tabIdxMap(rs.int(1)),
                monitor=Monitor.withName(rs.string(2)),
                dataTime = rs.timestamp(3),
                monitorType = MonitorType.withName(rs.string(4)),
                modified_time = rs.timestamp(5),
                changed_status = rs.string(6),
                operator = rs.string(7),
                reason = rs.stringOpt(8)
              )}.single.apply        
    }
  }
  
  def getLogs(tabType:TableType.Value, monitors:Seq[Monitor.Value], start:Timestamp, end:Timestamp)={
    DB readOnly{
      val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
      implicit session =>
        sql"""
          Select *
          From manualAuditLog
          Where tabType = ${tabTypeToIdMap(tabType)} and monitor in ${mStr} and dataTime>=${start} and dataTime<${end} 
          """.map { 
          rs => ManualAuditLog(tabType=tabIdxMap(rs.int(1)),
                monitor=Monitor.withName(rs.string(2)),
                dataTime = rs.timestamp(3),
                monitorType = MonitorType.withName(rs.string(4)),
                modified_time = rs.timestamp(5),
                changed_status = rs.string(6),
                operator = rs.string(7),
                reason = rs.stringOpt(8)
              )}.list().apply       
    }
  }
  def updateLog(log:ManualAuditLog)={
    DB localTx{
      val t:java.sql.Timestamp = log.dataTime
      val now:java.sql.Timestamp = DateTime.now
      implicit session =>
        sql"""
          Update manualAuditLog
          Set modified_time = ${now},
                changed_status = ${log.changed_status},
                operator = ${log.operator},
                reason = ${log.reason}
          Where tabType = ${tabTypeToIdMap(log.tabType)} and monitor=${log.monitor.toString()} and dataTime=${t} and monitorType=${log.monitorType.toString}
          """.update.apply
    }
  }
 
  def deleteLog(tabType:TableType.Value, monitor:Monitor.Value, dataTime:Timestamp, monitorType:MonitorType.Value)={
    DB localTx{
      implicit session =>
        sql"""
          DELETE FROM [dbo].[manualAuditLog]
          Where tabType = ${tabTypeToIdMap(tabType)} and monitor=${monitor.toString} and dataTime=${dataTime} and monitorType=${monitorType.toString}
          """.update.apply       
    }
  }
  
  def newLog(log:ManualAuditLog)={
    DB localTx {
      implicit session =>
        val dataTime:Timestamp = log.dataTime
        val modified_time:Timestamp = log.modified_time
        
        sql"""
          INSERT INTO [dbo].[manualAuditLog]
           ([tabType]
           ,[monitor]
           ,[dataTime]
           ,[monitorType]
           ,[modified_time]
           ,[changed_status]
           ,[operator]
           ,[reason])
     VALUES
           (${tabTypeToIdMap(log.tabType)}
           ,${log.monitor.toString()}
           ,${dataTime}
           ,${log.monitorType.toString}
           ,${modified_time}
           ,${log.changed_status}
           ,${log.operator}
           ,${log.reason})
          """.update.apply
    }
  }
}