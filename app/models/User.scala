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
case class UserSetting(widgets:Option[Seq[MonitorType.Value]], smsNotification:Option[Boolean])
case class User(id: Option[Int], email: String, password: String, name: String, 
    phone: String, isAdmin: Boolean, groupID: Int, alarmConfig:Option[AlarmConfig], setting:Option[UserSetting]){
  val myWidget={
    if(setting.isEmpty || setting.get.widgets.isEmpty)
      Seq.empty[MonitorType.Value]
    else{
       val group = Group.getGroup(groupID).get
       val widgets = setting.get.widgets.get
       group.privilege.allowedMonitorTypes.filter { widgets.contains }
    }
  }
}

object User {
  val defaultUserSetting = UserSetting(Some(Seq.empty[MonitorType.Value]), Some(false))
  implicit val settingRead = Json.reads[UserSetting]
  implicit val settingWrite = Json.writes[UserSetting]
  
  def newUser(user: User)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val alarmConfig = Json.toJson(AlarmConfig.defaultConfig).toString
      sql"""
        Insert into Users(email, name, password, phone, isAdmin, groupID, alarmConfig, setting)
        Values(${user.email}, ${user.name}, ${user.password}, ${user.phone}, 
          ${user.isAdmin}, ${user.groupID}, ${alarmConfig}, ${Json.toJson(defaultUserSetting).toString})
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
          groupID=${user.groupID}, alarmConfig=${alarmConfig}, setting=${Json.toJson(user.setting).toString}
        Where id=${user.id.get}  
        """.update.apply
    }
  }

  def userMapper = { (r: WrappedResultSet) =>
    val alarmConfig = if (r.stringOpt("alarmConfig").isEmpty)
      AlarmConfig.defaultConfig
    else
      Json.parse(r.string("alarmConfig")).validate[AlarmConfig].get

    val userSetting = if (r.stringOpt("setting").isEmpty)
      defaultUserSetting
    else
      Json.parse(r.string("setting")).validate[UserSetting].get

    User(Some(r.int("ID")), r.string("Email"),
      r.string("Password"), r.string("Name"), r.string("Phone"), r.boolean("isAdmin"),
      r.int("GroupID"), Some(alarmConfig), Some(userSetting))
  }
  
  def getUserById(id: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      Where id=${id}
      """.map {userMapper}.single.apply()
  }

  def getUserByEmail(email: String)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      Where email=${email}
      """.map { userMapper}.single.apply()
  }

  def getAllUsers()(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      """.map {userMapper}.list.apply()
  }

  def getAdminUsers()(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Users
      Where isAdmin = 1
      """.map { userMapper}.list.apply()
  }
  
  def getCountGroupID(groupID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select Count(GroupID)
      From Users
      Where GroupID=${groupID}
      """.map { r => r.int(1) }.single().apply().get
  }
  
  
}
