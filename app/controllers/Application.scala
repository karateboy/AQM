package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._

object Application extends Controller {

  val title = "麥寮廠區空氣品質及氣象監測系統"
  
  def index = Security.Authenticated{
    implicit request =>
    Ok(views.html.index(title))
  }
}
