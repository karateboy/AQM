package models
import play.api._
import play.api.mvc._
import models.ModelHelper._
import scala.collection.Map
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._

import EnumUtils._

object SystemConfig extends Enumeration {
  val AutoAuditAsNormal = Value

  var map = {
    val configPair =
      DB readOnly {
        implicit session =>

          sql"""
        Select * 
        From SystemConfig
        """.map { r => (SystemConfig.withName(r.string(1)) -> r.string(2)) }.list.apply
      }
    Map(configPair: _*)
  }

  def getConfig(key:SystemConfig.Value, default:String)={
    val opt = map.get(key)
    if(opt.isDefined)
      opt.get
    else{
      Logger.error(s"${key.toString} is not in SystemConfig")
      default
    }      
  }
  
  
  def setConfig(key:SystemConfig.Value, value:String)={
    map = (map - key) + (key->value)
    DB localTx {
      implicit session =>
        sql"""
          UPDATE SystemConfig
          SET [value] = ${value}
          WHERE configKey = ${key.toString}
          """.update.apply
    }
  }
}