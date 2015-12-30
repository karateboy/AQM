package models
import play.api._
import com.cht.messaging.sns._;
import java.io._;

object SmsSender {
    val strIp = "203.66.172.133"
    val nPort = 8001
    val account = "10358"
    val password = "10358"

  def submitMsg(phoneNoList: List[String], text: String): Boolean =
    {
      val sns = new SnsClient_V1(strIp, nPort)
      val sdm = new SubmitDataModel()
      // Login server
      sdm.setType(IConstants.SERV_LOGIN)
      sdm.setAccount(account)
      sdm.setPassword(password)
      if (sns.login(sdm) == false) {
        Logger.error("login server fail(" + sns.getLastMessage() + ")")
        sns.logout()
        return false
      }
      
      var noError = true
      for (phoneNo <- phoneNoList) {
        sdm.reset();
        sdm.setType(IConstants.SERV_SUBMIT_MSG);
        sdm.setCoding(1);
        sdm.setRcvMsisdn(phoneNo);
        sdm.setTranType(IConstants.DEFAULT_TRAN_TYPE);
        sdm.setLength(text.getBytes().length.toByte);
        sdm.setMessage(text);
        val rdm = sns.submitMessage(sdm);
        if (rdm != null) {
          if (rdm.getCode != 0) {
            Logger.error(new String(rdm.getDesc()).trim())
            noError = false
          }
        } else // could not receive response from SNS server
        {
          Logger.error("Submit fail:" + sns.getLastMessage())
          noError = false
        }
      }
      
      // Logout server
      sns.logout();
      return noError
    }

    def send(users:List[User], msg:String){
      val phones = users.filter { u => u.setting.isDefined && u.setting.get.smsNotification.isDefined && u.setting.get.smsNotification.get }.map { _.phone }
      submitMsg(phones, msg);
    }
}