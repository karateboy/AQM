package models
import play.api._
import hiairv2._

object SmsSender {
  
    def send(users:List[User], msg:String){
      val mySms = new sms2()

      val server = "api.hiair.hinet.net"
      val port = 8000
      
      if(mySms.create_conn(server, port, "0911510358", "10358") != 0){
        Logger.error(s"無法建立連線! ${mySms.get_message}")
        return;
      }
      for(user<-users){
        val ret = mySms.send_text_message(user.phone, msg);
        if(ret != 0){
          Logger.error(s"簡訊傳送發生錯誤! ret=${ret} ${mySms.get_message}")
          mySms.close_conn()
          return
        }
      }
      mySms.close_conn()      
    }
}