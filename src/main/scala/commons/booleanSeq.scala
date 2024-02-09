package commons

class BooleanSeq(size :Int) extends collection.mutable.IndexedSeq[Boolean]:
  private val byteArray = new Array[Byte](size/8 + 1)

  def apply(idx: Int): Boolean = (1 & (byteArray(idx/8) >> (idx%8))) != 0

  def update(index: Int, elem: Boolean): Unit =
    val t = 1 << (index%8)
    val b = index/8
    val f = (x: Int) => if elem then x | t else x & ~t
    byteArray(b) = f(byteArray(b)).toByte

  val length: Int = size
end BooleanSeq

object BooleanSeq:
  def fill(size: Int, v: Boolean = false): BooleanSeq =
    val ans = new BooleanSeq(size)
    val fill = (if v then -1 else 0).toByte
    for (i <- 0 to size/8) do
      ans.byteArray(i) = fill
    ans
end BooleanSeq
