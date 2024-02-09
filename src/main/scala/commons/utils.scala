package commons

import scala.annotation.tailrec
import scala.collection.mutable

//def main(args :Array[String]): Unit = {}
/*println(factor(factorial(1000)))
    println("start")
    for (i <- 0 until 2000) {
      if (factorial(i) != Old.factorial(i)) {
        println("oh no")
      }
      if (i % 1000 == 0) {
        println(i/1000)
      }
    }
    println("done")
    def race(n :Int) {
      print(clock(n, factorial)+" ")
      print(clock(n, Old.factorial1)+" ")
      println()
    }
    var i = 128
    while(i < 1000000) {
      race(i)
      i = i * 2 toInt
    }*/

def moi[A](f: (Int => A) => Int => A)(n: Int): A =
  val mem = Array.fill[Option[A]](n + 1)(None)
  def mf(n: Int): A = mem(n) match
    case _: None.type =>
      val v = f(mf)(n)
      mem(n) = Some(v)
      v
    case Some(v) => v
  mf(n)

def isPandigital(n: BigInt, m: Int = 9) =
  val mem = new Array[Boolean](m + 1)
  mem(0) = true
  @tailrec
  def loop(ds: List[Int], c: Int): Boolean =
    if ds == Nil then c == m
    else
      val h = ds.head
      if mem(h) then false
      else
        mem(h) = true
        loop(ds.tail, c + 1)
  loop(xits(n), 0)

def xits(n: BigInt, b: Int = 10): List[Int] =
  @tailrec
  def helper(n: BigInt, acc: List[Int]): List[Int] =
    if n == 0 then acc
    else helper(n / b, (n % b).toInt :: acc)
  helper(n, Nil)

def max[A: Ordering](l: IndexedSeq[A]) =
  import scala.math.Ordered.*
  var argmax = 0
  var max    = l(0)
  for i <- 1 until l.size do
    if l(i) > max then
      argmax = i
      max = l(argmax)
  (argmax, max)

def isSquare(n: Long) =
  val root = math.sqrt(n.toDouble).round
  root * root == n

def isSquare(n: BigInt) =
  val root = approxSqrt(n)
  root * root == n

def approxSqrt(n: BigInt) =
  @tailrec
  def find(mi: BigInt, ma: BigInt): BigInt =
    if ma - mi < 2 then mi
    else
      val me = (mi + ma) / 2
      if me * me <= n then find(me, ma) else find(mi, me)
  val mi = 1 << (n.bitLength - 1) / 2
  find(mi, 2 * mi)

def approxSqrt(n: Long) = math.sqrt(n.toDouble).round

def isPalindrome[A](l: IndexedSeq[A])(using CanEqual[A, A]) =
  val size = l.size
  @tailrec
  def loop(i: Int): Boolean =
    if i > size / 2 then true
    else if l(i) != l(size - i - 1) then false
    else loop(i + 1)
  loop(0)

def factorial(n: Int): BigInt =
  def facTail(m: Long, n: Int) =
    if n - m == 2 then m * (m + 1) * (m + 2) else m * (m + 1)
  def fact(m: Int, n: Int): BigInt =
    if n - m < 3 then facTail(m, n)
    else
      val middle = (m + n) / 2
      fact(middle + 1, n) * fact(m, middle)
  if n < 2 then 1
  else fact(1, n)

@tailrec
def gcd(a: Int, b: Int): Int =
  if b == 0 then a
  else gcd(b, a % b)

def gcd(h: Int, l: List[Int]): Int =
  @tailrec
  def helper(l: List[Int], acc: Int): Int = l match
    case Nil     => gcd(h, acc)
    case n :: ns => helper(ns, gcd(n, acc))
  helper(l, 0)

def stix(l: List[Int], b: Int = 10) =
  @tailrec
  def helper(l: List[Int], n: BigInt): BigInt = l match
    case Nil    => n
    case d :: m => helper(m, b * n + d)
  helper(l, 0)

def parse[A](input: String = "input.txt"): String =
  val file = io.Source.fromFile(input)
  val result = file.mkString
  file.close()
  result

//  def parseMatrix[N](numParser: String => N): Seq[Seq[N]] = {
//    parseInput(_ split "\\s" map numParser)
//  }
//
//  def parseInput[A](lineParser :String => A) :Seq[A] = {
//    io.Source.fromFile("input.txt").mkString.split("\\n") map
//      (_.trim()) map lineParser
//  }

def factorSizes(n: Int) =
  val sq  = math.sqrt(n).toInt
  var ans = List[Int]()
  var q   = n
  var d   = 2
  while d <= sq do
    var m = 0
    while q % d == 0 do
      m += 1
      q /= d
    if m != 0 then ans ::= m
    d += 1
  if q != 1 then ans ::= 1
  ans

def divisors(n: BigInt) =
  var r      = n
  var result = Vector[BigInt](1)
  def sqrtUpperBound(n: BigInt) =
    (1: BigInt) << ((n.bitLength + 1) / 2)
  var s = sqrtUpperBound(n)
  var i = 2: BigInt
  while i < s do
    var temp = result
    while r % i == 0 do
      r = r / i
      temp = temp map (_ * i)
      result ++= temp
    i += 1
    s = sqrtUpperBound(r)
  if r != 1 then
    result ++= result map (_ * r)
  result

def primeBitMap(n: Int) =
  val seed = primes(math.sqrt(n).ceil.toInt)
  val ans  = BooleanSeq.fill(n, v = true)
  ans(0) = false
  ans(1) = false
  for (p <- seed) do
    var m = 2 * p
    while m < n do
      ans(m) = false
      m += p
  ans

/** implements the Euler sieve until n (excluded)
  */
def primes(n: Int) =
  val sq   = math.sqrt(n).ceil.toInt
  val prev = (-1 until n - 1).toArray
  val next = (1 until n + 1).toArray
  def remove(i: Int): Unit =
    if prev(i) >= 0 then next(prev(i)) = next(i)
    if next(i) < n then prev(next(i)) = prev(i)
    next(i) = -1
  var primes  = IndexedSeq[Int]()
  var current = 2
  while current < sq do
    primes :+= current
    var curr = (n - 1) / current
    while next(curr) == -1 do
      curr = prev(curr)
    while curr >= current do
      val i = current * curr
      remove(i)
      curr = prev(curr)
    current = next(current)
  while current < n do
    primes :+= current
    current = next(current)
  primes

def factorSizesInRange(start: Int, end: Int, primes: Iterable[Int]) =
  val quotient = (start until end).toArray
  val divisors = Array.fill[List[Int]](end - start)(Nil)
  def pluck(p: Int): Unit =
    var i = p - (start - 1) % p - 1
    while i < end - start do
      quotient(i) /= p
      var m = 1
      while quotient(i) % p == 0 do
        quotient(i) /= p
        m += 1
      divisors(i) ::= m
      i += p
  val bound = math.sqrt(end).ceil.toInt
  for (p <- primes.view.takeWhile(_ < bound)) do pluck(p)
  for (i <- 0 until end - start) do
    if quotient(i) != 1 then divisors(i) ::= 1
  divisors

def factor(n: BigInt, m: BigInt = 2): List[(BigInt, Int)] =
  @tailrec
  def factorHelp(n: BigInt, m: BigInt): List[(BigInt, Int)] =
    @tailrec
    def loop(r: BigInt, i: Int): (BigInt, Int) =
      if r % m == 0 then loop(r / m, i + 1)
      else (r, i)
    if n % m == 0 then
      val (r, i) = loop(n / m, 1)
      (m, i) :: factor(r, m + 1)
    else factorHelp(n, m + 1)
  if n < 2 then List()
  else if n.isProbablePrime(128) then List((n, 1))
  else factorHelp(n, m)

def factorInRange(
  start: Long,
  end: Long
)(primes: Iterable[Int] = this.primes(math.sqrt(end.toDouble).ceil.toInt)): Array[List[(Long, Int)]] =
  val quotient = (start until end).toArray
  val size     = (end - start).toInt
  val factors  = Array.fill[List[(Long, Int)]](size)(Nil)
  def sweep(p: Int): Unit =
    var i = p - ((start - 1) % p).toInt - 1
    while i < size do
      quotient(i) /= p
      var m = 1
      while quotient(i) % p == 0 do
        quotient(i) /= p
        m += 1
      factors(i) ::= (p, m)
      i += p
  val bound = math.sqrt(end.toDouble).ceil.toInt
  for (p <- primes.view.takeWhile(_ < bound)) do
    sweep(p)
  for (i <- 0 until size) do
    if quotient(i) != 1 then
      factors(i) ::= (quotient(i), 1)
    factors(i) = factors(i).reverse
  factors

def divisorCount(n: BigInt) =
  var r      = n
  var result = 1: BigInt
  def sqrtUBound(n: BigInt) =
    (1: BigInt) << ((n.bitLength + 1) / 2)
  var s = sqrtUBound(n)
  var i = 2: BigInt
  while i < s do
    var m = 1
    while r % i == 0 do
      r = r / i
      m += 1
    result *= m
    i += 1
    s = sqrtUBound(r)
  if r != 1 then
    result *= 2
  result

/** clocks a function of one argument
  */
def clock[A](x: A, f: A => ?, time: Int = 1000) =
  var n     = 1
  val start = System.currentTimeMillis()
  f(x)
  val stop      = System.currentTimeMillis()
  var timeTaken = stop - start
  while timeTaken < time do
    System.gc()
    val start = System.currentTimeMillis()
    for (i <- 0 until n) do
      f(x); f(x); f(x); f(x)
    val stop = System.currentTimeMillis()
    timeTaken = stop - start
    n *= 4
  timeTaken.toDouble / n

// vvv === OLD === vvv

def factorRange(start: BigInt, size: Int, ps: IndexedSeq[Int] = primes(128)) =
  val bound = ps.last + 2
  val end   = start + size
  val lim   = math.sqrt(end.toDouble)
  if end.isValidLong && lim < bound then
    factorInRange(start.toLong, end.toLong)(ps)
  else
    val quotient = (start until end).toArray
    val factors  = Array.fill[List[(BigInt, Int)]](size)(Nil)
    def sweep(p: Int): Unit =
      var i = p - ((start - 1) % p).toInt - 1
      while i < size do
        quotient(i) /= p
        var m = 1
        while quotient(i) % p == 0 do
          quotient(i) /= p
          m += 1
        factors(i) ::= (p, m)
        i += p
    for (p <- ps.view.takeWhile(_ < bound)) do
      sweep(p)
    for (i <- 0 until size) do
      if quotient(i) != (1: BigInt) then
        factors(i) = factor(quotient(i), bound).reverse ++ factors(i)
    factors

//def factorial(n :Int) :BigInt =
//  if (n < 2) then 1
//  else n * factorial(n-1)

def factorial0(n: Int) =
  var ans: BigInt = 1
  var m           = 2
  while m <= n do
    ans *= m
    m += 1
  ans

def factorial1(n: Int): BigInt =
  @tailrec
  def fact(n: Int, acc: BigInt): BigInt =
    if n < 2 then 1
    else fact(n - 1, n * acc)
  fact(n, 1)

def factorial4(n: Int) =
  @tailrec
  def fact(n: Long, acc: BigInt): BigInt =
    if n < 16 then acc * factorial3(n.toInt)
    else
      val end = (math.log(Long.MaxValue) / math.log(n.toDouble)).toInt
      var m = n
      for (i <- n - end + 1 until n) do m *= i
      fact(n - end, m * acc)
  fact(n, 1)

def factorial3(n: Int): BigInt =
  @tailrec
  def fact(n: Long, acc: BigInt): BigInt =
    if n < 8 then acc * factorial2(n.toInt)
    else
      (math.log(Long.MaxValue) / math.log(n.toDouble)).toInt match
        case 1 => fact(n - 1, n * acc)
        case 2 => fact(n - 2, n * (n - 1) * acc)
        case 3 => fact(n - 3, n * (n - 1) * (n - 2) * acc)
        case 4 => fact(n - 4, n * (n - 1) * (n - 2) * (n - 3) * acc)
        case 5 => fact(n - 5, n * (n - 1) * (n - 2) * (n - 3) * (n - 4) * acc)
        case 6 => fact(n - 6, n * (n - 1) * (n - 2) * (n - 3) * (n - 4) * (n - 5) * acc)
        case 7 => fact(n - 7, n * (n - 1) * (n - 2) * (n - 3) * (n - 4) * (n - 5) * (n - 6) * acc)
        case _ => fact(n - 8, n * (n - 1) * (n - 2) * (n - 3) * (n - 4) * (n - 5) * (n - 6) * (n - 7) * acc)
  fact(n, 1)

def factorial2(n: Int) =
  @tailrec
  def fact(n: Long, acc: BigInt): BigInt =
    if n < 2 then acc
    else fact(n - 2, n * (n - 1) * acc)
  fact(n, 1)

def approxSqrt2(n: BigInt) =
  @tailrec
  def find(m: BigInt): BigInt =
    val v = (m + n / m) / 2
    if (v - m).abs <= 1 then (v + n / v) / 2
    else find(v)
  find(1 << (n.bitLength - 1) / 2)

def isPandigital2(n: BigInt, m: Int = 9) =
  toBag(xits(n)) == toBag(1 to m)

def toBag[A](l: Iterable[A]) =
  val ans: mutable.Map[A, Int] = new mutable.HashMap[A, Int]
  for a <- l do
    if ans.contains(a) then ans(a) += 1
    else ans(a) = 1
  ans
