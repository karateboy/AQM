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

case class User(id: Option[Int], email: String, password: String, name: String, 
    phone: String, isAdmin: Boolean, groupID: Int, alarmConfig:Option[AlarmConfig])

object User {
  def newUser(user: User)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val alarmConfig = Json.toJson(AlarmConfig.defaultConfig).toString
      sql"""
        Insert into Users(email, name, password, phone, isAdmin, groupID, alarmConfig)
        Values(${user.email}, ${user.name}, ${user.password}, ${user.phone}, 
          ${user.isAdmin}, ${user.groupID}, ${alarmConfig})
        """.update.apply
    }
  }

  def deleteUser(id: Integer)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Delete from Users
        Where id = ${id}
        """.update.apply
    }
  }

  def updateUser(user: User)(implicit session: DBSession = AutoSession) = {
    assert(user.id.isDefined)
    DB localTx { implicit session =>
      val alarmConfig = 
        if(user.alarmConfig.isEmpty)
          None
        else
          Some(Json.toJson(user.alarmConfig).toString)

      sql"""
        Update Users
        Set email=${user.email}, name=${user.name}, password=${user.password}, phone=${user.phone}, isAdmin=${user.isAdmin}, 
          groupID=${user.groupID}, alarmConfig=${alarmConfig}
        Where id=${user.id.get}  
        """.update.apply
    }
  }

  def getUserById(id: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      Where id=${id}
      """.map { r =>
      val alarmConfig = if (r.stringOpt("alarmConfig").isEmpty)
        AlarmConfig.defaultConfig
      else
        Json.parse(r.string("alarmConfig")).validate[AlarmConfig].get

      User(Some(r.int("ID")), r.string("Email"),
        r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),
        r.int("GroupID"), Some(alarmConfig))
    }.single.apply()
  }

  def getUserByEmail(email: String)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      Where email=${email}
      """.map { r =>
      val alarmConfig = if (r.stringOpt("alarmConfig").isEmpty)
        AlarmConfig.defaultConfig
      else
        Json.parse(r.string("alarmConfig")).validate[AlarmConfig].get
  
      User(Some(r.int("ID")), r.string("Email"),
        r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),
        r.int("GroupID"), Some(alarmConfig))
    }.single.apply()
  }

  def getAllUsers()(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      """.map { r =>
      val alarmConfig = if (r.stringOpt("alarmConfig").isEmpty)
        AlarmConfig.defaultConfig
      else
        Json.parse(r.string("alarmConfig")).validate[AlarmConfig].get
  
      User(Some(r.int("ID")), r.string("Email"),
        r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),
        r.int("GroupID"), Some(alarmConfig))
    }.list.apply()
  }

  def getAdminUsers()(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      Where isAdmin = 1
      """.map { r =>
      val alarmConfig = if (r.stringOpt("alarmConfig").isEmpty)
        AlarmConfig.defaultConfig
      else
        Json.parse(r.string("alarmConfig")).validate[AlarmConfig].get
  
      User(Some(r.int("ID")), r.string("Email"),
        r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),
        r.int("GroupID"), Some(alarmConfig))
    }.list.apply()
  }
  
  def getCountGroupID(groupID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select Count(GroupID)
      From Users
      Where GroupID=${groupID}
      """.map { r => r.int(1) }.single().apply().get
  }
}
