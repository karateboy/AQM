package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json
import Privilege._

case class Group(id: Option[Int], name: String, privilege: Privilege)
object Group {
  def newGroup(group: Group)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Insert into Groups(name, privilege)
        Values(${group.name}, ${Json.toJson(group.privilege).toString})
        """.update.apply
    }
  }

  def deleteGroup(id: Int)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Delete from Groups
        Where id = ${id}
        """.update.apply
    }
  }

  def updateGroup(group: Group)(implicit session: DBSession = AutoSession) = {
    assert(group.id.isDefined)
    DB localTx { implicit session =>
      sql"""
        Update Groups
        Set name=${group.name},privilege=${Json.toJson(group.privilege).toString}
        Where id=${group.id.get}  
        """.update.apply
    }
  }

  def getAllGroups()(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Groups
      """.map { r =>
        try{
          Group(Some(r.int(1)), r.string(2), Json.parse(r.string(3)).validate[Privilege].get)
        }catch{
          case e:Throwable=>
            Group(Some(r.int(1)), r.string(2), Privilege.defaultPrivilege)
        }
    }.list.apply()
  }

  def getGroup(id: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Groups
      Where id=${id}
      """.map { r =>
        try{
          Group(Some(r.int(1)), r.string(2), Json.parse(r.string(3)).validate[Privilege].get)
        }catch{
          case e:Throwable=>
            Group(Some(r.int(1)), r.string(2), Privilege.defaultPrivilege)
        }
    }.single.apply()
  }
}