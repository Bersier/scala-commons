
class BooleanSeq(size :Int) extends collection.mutable.IndexedSeq[Boolean] {
  private val byteArray = new Array[Byte](size/8 + 1)

  def apply(idx: Int): Boolean = (1 & (byteArray(idx/8) >> (idx%8))) != 0

  def update(index: Int, elem: Boolean) {
    val t = 1 << (index%8)
    val b = index/8
    val f = (x :Int) => elem match {
      case false => x & ~t
      case true =>  x |  t
    }
    byteArray(b) = f(byteArray(b)) toByte
  }

  val length: Int = size
}

object BooleanSeq {
  def fill(size :Int, v :Boolean = false) = {
    val ans = new BooleanSeq(size)
    val fill = (if (v) -1 else 0) toByte;
    for (i <- 0 to size/8) {
      ans.byteArray(i) = fill
    }
    ans
  }
}