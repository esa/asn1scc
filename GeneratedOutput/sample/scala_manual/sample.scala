import stainless.annotation._
import stainless.math.BitVectors._


case class Ref[T](var x: T) {}

@cCode.export
case class Message_szDescription(
  var arr: Array[Int]
) {
  //require(arr.length == 10)
  
  def Message_szDescription_Initialize(): Unit = {
  	arr = Array.fill(10)(0)
  }

  def Message_szDescription_IsConstraintValid(pErrCode: Ref[Int]): Boolean = {
    var ret = true
    pErrCode.x = 0
    return ret
  }
}

@cCode.export
case class Message (
  var msgId: Int = 0,
  var myflag: Int = 0,
  var szDescription: Message_szDescription,
  var isReady: Boolean
) {
  
  def Message_Initialize(): Unit = {
  	msgId = 0
  	myflag = 0
  	this.szDescription.Message_szDescription_Initialize()
  	isReady = false
  }

  def Message_IsConstraintValid(pErrCode: Ref[Int]): Boolean = {
    var ret = true
    ret = this.szDescription.Message_szDescription_IsConstraintValid(pErrCode)
    return ret
  }
}
