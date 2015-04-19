package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._

object Monitor extends Enumeration{
  val A001 = Value("A001")
  val A002 = Value("A002")
  val A003 = Value("A003")
  val A004 = Value("A004")
  val A005 = Value("A005")
  val A006 = Value("A006")
  val A007 = Value("A007")
  val A008 = Value("A008")
  val A009 = Value("A009")
  val A010 = Value("A010")
  val A011 = Value("A011")
  val A012 = Value("A012")
  val A013 = Value("A013")
  
  val map = Map[Value, String](
    A001->"大城站",
    A002->"太保站",
    A003->"東石站",
    A004->"I棟站",
    A005->"褒忠站",
    A006->"崙背站",
    A007->"四湖站",
    A008->"東勢站",
    A009->"麥寮站",
    A010->"台西站",
    A011->"土庫站",
    A012->"西螺站",
    A013->"監測車"
  )
  
  
  def getDisplayName(monitor:Value):String ={
    map.get(monitor) match{
      case Some(name) => name 
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

object MonitorType extends Enumeration{
  val A213 = Value(1)
  val A214 = Value
  val A215 = Value
  val A221 = Value
  val A222 = Value
  val A223 = Value
  val A224 = Value
  val A225 = Value
  val A226 = Value
  val A229 = Value
  val A232 = Value
  val A233 = Value
  val A235 = Value
  val A283 = Value
  val A286 = Value
  val A288 = Value
  val A289 = Value
  val A293 = Value
  val A296 = Value
  val C211 = Value
  val C212 = Value
  val C213 = Value
  val C214 = Value
  val C215 = Value
  val C216 = Value
  val C911 = Value
  val C912 = Value
  
  val map = Map[Value, String](
     A213->"TSP",
     A214->"PM10",
     A215->"PM25",
     A221->"總硫",
     A222->"二氧化硫",
     A223->"氮氧化物",
     A224->"一氧化碳",
     A225->"周界臭氧 ",
     A226->"碳氫化物",
     A229->"氨",
     A232->"NOY",
     A233->"NOY-NO",
     A235->"NH3",
     A283->"一氧化氮",
     A286->"甲烷",
     A288->"站房濕度",
     A289->"站房溫度",
     A293->"二氧化氮",
     A296->"非甲烷",
     C211->"風速",
     C212->"風向",
     C213->"雨量",
     C214->"溫度",
     C215->"濕度",
     C216->"氣壓",
     C911->"六秒風速",
     C912->"六秒風向"
   )
}