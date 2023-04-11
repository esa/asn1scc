import stainless.annotation._

object foo {

case class Ref[T](var x: T) {}

case class Message (
    anInt: Int,
	anFloat: Float,
    aBool: Boolean,
) {}


@cCode.export
def Message_Initialize(a: Ref[GlobalState]): Message =
{
	Message(
		0, 
		false,
		Inner(42)
  )
}

def abc(pVal: Ref[Array[Byte]], pErrCode: Ref[Int]): Boolean = {
  var ret: Boolean = true;
  ret = true;
  pErrCode.x = 0;

  ret;
}


	
}