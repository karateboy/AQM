package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._

object Monitor extends Enumeration{
  case class Monitor(id:String, name:String, lat:Double, lng:Double)
  val monitorList:List[Monitor] =
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From Monitor
        """.map { r => Monitor(r.string(1), r.string(2), r.string(6).toDouble, r.string(7).toDouble)}.list.apply
    }
    
  val map:Map[Value, Monitor] = Map(monitorList.map{e=>Value(e.id)->e}:_*)

  def getDisplayName(monitor:Value):String ={
    map.get(monitor) match{
      case Some(m) => m.name 
      case None => 
        Logger.error("Unknown monitor :" + monitor)
        ""
    }
  }
  
  def main(args: Array[String]) {
    for(p<-MonitorType.values.toList){
        println(p + ":" + p.id + ":" + MonitorType.map.get(p).get)
    }
      
  }
  
  def insertHourlyReport(monitor:Value, date:Date)={
 
  }
}

case class MonitorType(id:String, desp:String, unit:String, 
    std_internal:Option[Float], std_law:Option[Float], std_hour:Option[Float],
    std_day:Option[Float], std_year:Option[Float])
    
object MonitorType extends Enumeration{
  val mtList:List[MonitorType] =
    DB readOnly{ implicit session =>
      sql"""
        Select *
        From MonitorType
      """.map { r =>  MonitorType(id = r.string(1), 
          desp = r.string(2),
          unit = r.string(3),
          std_internal = r.floatOpt(5),
          std_law = r.floatOpt(6), 
          std_hour = r.floatOpt(7),
          std_day = r.floatOpt(8), 
          std_year = r.floatOpt(9)
          )}.list.apply
    }
  
  val map:Map[Value, MonitorType] = Map(mtList.map{e=>Value(e.id)->e}:_*)
  val mtvList = mtList.map(mt=>MonitorType.withName(mt.id))
}
