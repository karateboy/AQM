package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._

object Alarm {

  case class Alarm(monitor:Monitor.Value, mItem:String, time:DateTime, mVal:Float, code:MonitorStatus.Value)
  def getAlarm(monitors:Seq[Monitor.Value], statusFilter:Option[Seq[MonitorStatus.Value]], start:DateTime, end:DateTime)(implicit session: DBSession = AutoSession) = {
    Logger.info("statusFilter=" + statusFilter)
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('","','","')"))
    val startT:Timestamp = start
    val endT:Timestamp = end
    val tab = getTabName(start.getYear) 
    if(statusFilter.isEmpty){
      sql"""
        Select *
        From ${tab}
        Where DP_NO in ${mStr} and M_DateTime>=${startT} and M_DateTime<${end}
        """.map{
          rs=>Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4), 
              MonitorStatus.withName(rs.string(5).trim()))
          }.list.apply
    }else{
      val sfilter = SQLSyntax.createUnsafely(statusFilter.get.mkString("('","','","')"))
      
      sql"""
        Select *
        From ${tab}
        Where DP_NO in ${mStr} and M_DateTime>=${startT} and M_DateTime<${end} and CODE2 in ${sfilter}
        """.map{
          rs=>Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4), 
              MonitorStatus.withName(rs.string(5).trim()))
          }.list.apply
    }
  }
  
  def getTabName(year: Int) = {
    SQLSyntax.createUnsafely(s"[AQMSDB].[dbo].[P1234567_Alm_${year}]")
  }
  
  case class AlarmItem(id:String, desp:String)
  private val arList:List[AlarmItem] =
    DB readOnly{ implicit session =>
      sql"""
        SELECT *
        FROM [AQMSDB].[dbo].[code1]
      """.map { r =>  AlarmItem(r.string(1), r.string(2))   
      }.list.apply
    } 
  val map:Map[String,String]= Map(arList.map{r=>(r.id->r.desp)}:_*)
}