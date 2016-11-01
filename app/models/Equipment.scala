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
    val map = Map.empty[Monitor.Value, Map[String, Equipment]]
    for(e <- equipments){
      val monitorMap = map.getOrElseUpdate(e.monitor, Map.empty[String, Equipment])
      monitorMap.put(e.name, e)
    }
    map
  }
  val map = generateMap
  
  def getEquipmentNameSet = {
    val nameSetIterable = map.values map {
      m =>
        m.keySet
    }
    
    nameSetIterable.fold(Set.empty[String])((s1, s2)=> s1 ++ s2)
  }
  def equipmentNameList = getEquipmentNameSet.toList.sorted
  
  
  def create(newEquip:Equipment)={
    DB localTx { implicit session =>
      sql"""
        Insert into Equipment(DP_NO, id, name, brand, model, serial, bought)
        Values(${newEquip.monitor.toString}, ${newEquip.id}, ${newEquip.name}, ${newEquip.brand}, ${newEquip.model}, ${newEquip.serial}, ${newEquip.bought})
        """.update.apply
    }
    
    import scala.collection.mutable.Map
    val monitor = newEquip.monitor
    val equipNameMap = map.getOrElseUpdate(monitor, Map.empty[String, Equipment])
    equipNameMap.put(newEquip.name, newEquip)
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

      val updatedEquipmentOpt =
        sql"""
          Select *
          From Equipment
          Where DP_NO=${m.toString} and id=${equip_id}
        """.map { r =>
        Equipment(Monitor.withName(r.string(1)), r.string(2), r.string(3), r.string(4), r.string(5), r.string(6), r.string(7))
      }.single.apply
      
      updatedEquipmentOpt map {
        equip =>
          val equipNameMap = map(equip.monitor)
          equipNameMap.put(equip.name, equip)
      }
    }
  }
  
  def delete(monitor:Monitor.Value, id:String)={
    DB localTx { implicit session =>
      sql"""
        Delete from Equipment
        Where DP_NO = ${monitor.toString} and id = ${id}
        """.update.apply
    }
    
    val equipNameMap = map(monitor)
    val matchedNamePairOpt = equipNameMap.find{
      p=>
        val name = p._1
        val equipment = p._2
        equipment.id == id
    }
    
    matchedNamePairOpt map {
      pair =>
        equipNameMap.remove(pair._1)
    }
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

