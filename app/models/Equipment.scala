package models
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Equipment(monitor:Monitor.Value, id:String, name:String, brand:String, model:String, serial:String, bought:String)
/**
 * @author user
 */
object Equipment {
  implicit val equipRead = Json.reads[Equipment]
  def getList = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Equipment
        """.map { r =>
        Equipment(Monitor.withName(r.string(1)), r.string(2), r.string(3), r.string(4), r.string(5), r.string(6), r.string(7))
      }.list.apply

    }
  }
  
  def generateMap()={
    import scala.collection.mutable.Map
    val equipments = getList
    val map = Map.empty[Monitor.Value, List[Equipment]]
    for(e <- equipments){
      val list = map.getOrElse(e.monitor, List.empty[Equipment])
      
      map.put(e.monitor, e::list)
    }
    map
  }
  var map = generateMap
  
  def create(newEquip:Equipment)={
    DB localTx { implicit session =>
      sql"""
        Insert into Equipment(DP_NO, id, name, brand, model, serial, bought)
        Values(${newEquip.monitor.toString}, ${newEquip.id}, ${newEquip.name}, ${newEquip.brand}, ${newEquip.model}, ${newEquip.serial}, ${newEquip.bought})
        """.update.apply
    }
    
    val monitor = newEquip.monitor
    val equipList = map.getOrElse(monitor, List.empty[Equipment])
    val newList = newEquip::equipList
    
    map = map + (monitor->newList)
  }
  
  def update(ids:Array[String], newValue:String) = {
    assert(ids.length == 3)
    val m = Monitor.withName(ids(0))
    val equip_id = ids(1)
    val colName = ids(2)
    
    DB localTx { implicit session =>
      val col = SQLSyntax.createUnsafely(s"${colName}")
      sql"""
        Update Equipment
        Set ${col}=${newValue}
        Where DP_NO=${m.toString} and id=${equip_id}  
        """.update.apply

      val newList =
        sql"""
          Select *
          From Equipment
          Where DP_NO=${m.toString}
        """.map { r =>
        Equipment(Monitor.withName(r.string(1)), r.string(2), r.string(3), r.string(4), r.string(5), r.string(6), r.string(7))
      }.list.apply
      
      map = (map + (m -> newList))
    }
  }
  
  def delete(monitor:Monitor.Value, id:String)={
    DB localTx { implicit session =>
      sql"""
        Delete from Equipment
        Where DP_NO = ${monitor.toString} and id = ${id}
        """.update.apply
    }
    
    val oldList = map.getOrElse(monitor, List.empty)
    val newList = oldList.filter { equip => !(equip.monitor == monitor && equip.id == id) }
    map = (map + (monitor -> newList))
  }

  def getEquipment(id: String) = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Equipment
        Where id=${id}
        """.map { r =>
        Equipment(Monitor.withName(r.string(1)), r.string(2), r.string(3), r.string(4), r.string(5), r.string(6), r.string(7))
      }.single.apply
    }
  }
}

