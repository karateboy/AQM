package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.Logger

case class UserInfo(account: String, password: String)

/**
 * @author user
 */
object Login extends Controller {
  implicit val readsUserInfo: Reads[UserInfo] = 
    ((__ \ "account").read[String] and (__ \ "password").read[String])(UserInfo.apply _)

  implicit val userInfoWrites: Writes[UserInfo] =
    ((JsPath \ "account").write[String] and 
        (JsPath \ "password").write[String])(unlift(UserInfo.unapply))
    
        
  def authenticate = Action(BodyParsers.parse.json){
    implicit request =>
      val userInfo = request.body.validate[UserInfo]      
      userInfo.fold(
          error=>{
            BadRequest(Json.obj("ok"->false, "msg"->JsError.toFlatJson(error)))
          }, 
          userInfo=>{
            if(userInfo.password == "abc123"){
              Ok(Json.obj("ok"->true))
              //Redirect(routes.Application.index)
            }else
              Ok(Json.obj("ok"->false, "msg"->"密碼或帳戶錯誤"))
          })
  }
  
  def prompt = Action{
    Ok(views.html.login())
  }
  
  
}