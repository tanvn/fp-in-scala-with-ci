package chapter4

object ExceptionThrower {

  def failingFn(i : Int ) : Int = {
    val y : Int = throw new Exception("fail")

    try {
      val x = 42 +5
      x+y
    } catch { case e : Exception => 43 }
  }

  def main(args: Array[String]): Unit = {
    failingFn(12)

  }



}
