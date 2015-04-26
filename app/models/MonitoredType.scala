package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._

/**
 * @author user
 */

case class MonitoredType(id:String, desp:String, unit:String, used:Boolean)

object MonitoredType {
  def init = {
    DB localTx { implicit session =>
      val count =
        sql"""
        Select Count(*)
        From MonitoredType
        """.map { _.int(1) }.single().apply()

      if (count.get == 0) {
        Logger.info("No MonitoredType. Create default entries")
        sql"""
          INSERT INTO MonitoredType
           ([DP_NO], [ITEM], [USED])
           Select Monitor.DP_NO, MonitorType.Item, 'True'
           From MonitorType, Monitor 
        """.update.apply()

      }
    }
  }
  
  def getMonitoredTypes(monitor : Monitor.Value) = {
    DB readOnly { implicit session =>
      val monitoredTypes = 
        sql"""
          Select MonitoredType.Item, MonitorType.Desp, MonitorType.Unit, MonitoredType.Used  
          From MonitoredType, MonitorType
          Where DP_NO = ${monitor.toString()} and MonitoredType.Item = MonitorType.Item
          """.map { r=>MonitoredType(r.string(1),r.string(2),r.string(3),r.boolean(4))}.list().apply()
          
      monitoredTypes
    }
  }
  
  def getUsedMonitoredType(monitor : Monitor.Value)(implicit session: DBSession = AutoSession) = {
    val used =
      sql"""
        Select MonitoredType.Item
        From MonitoredType
        Where DP_NO=${monitor.toString()} and Used = 'True'
        """.map { rs=>MonitorType.withName(rs.string(1))}.list.apply
    Logger.debug("used #=" + used.length)
    used
  }
  
  def setMonitoredType(monitor:Monitor.Value, monitorType:MonitorType.Value, used:Boolean) = {
    DB localTx { implicit session =>
      sql"""
        Update MonitoredType
        Set Used = ${used}
        Where DP_NO=${monitor.toString()} and Item = ${monitorType.toString()}
        """.update.apply
    }
  }
}
