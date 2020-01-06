package doodle

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Random

object Evo extends App {

  printTest()

  def printTest(): Unit = {
    // negative Rastrigin function (see https://en.wikipedia.org/wiki/Test_functions_for_optimization)
    def testFunction(a: Double)(x: Array[Double]): Double = {
      -(a * x.length + x.map(xi => xi * xi - a * math.cos(2 * math.Pi * xi)).sum)
    }

    def cauchyNoise(amplitude: Double): Double = {
      amplitude * math.tan(math.Pi * (Random.nextDouble() - .5))
    }

    def randomInitialMutationRate() = math.abs(cauchyNoise(1.0 / 16))

    def mutated(x: (Double, Array[Double])): (Double, Array[Double]) = {
      val (mutationRate, point) = x
      val newMutationRate = if (Random.nextDouble() >= 1.0 / 128) {
        math.abs(mutationRate + cauchyNoise(mutationRate / 256))
      }
      else randomInitialMutationRate()
      val newPoint = point.map(xi => xi + cauchyNoise(mutationRate))
      (newMutationRate, newPoint)
    }

    val populationSize = 128
    val initialPopulation = Array.fill(populationSize)(
      (randomInitialMutationRate(), Array.fill(8)(cauchyNoise(16)))
    )
    val eval: Array[Double] => Double = testFunction(10)
    val initialFitness: Array[Double] = initialPopulation.map(_._2).map(eval)

    val champions = evolveToFitness(initialPopulation.zip(initialFitness).toMap)(
      target = -1.0 / 16,
      g => eval(g._2),
      mutated
    )

    println()
    for (champion <- champions) {
      println("Fitness: " + champion._2 + ", mutation rate: " + champion._1._1 + ", point: " + champion._1._2.toList)
    }
  }

  var generationCount = 0

  def evolveToFitness[Genome](population: Map[Genome, Double])
                             (implicit target: Double,
                              eval: Genome => Double,
                              mutator: Genome => Genome): Map[Genome, Double] = {
    generationCount += 1
    val populationSize = population.size
    println("Generation " + generationCount + ", Population size: " + populationSize +
      ", Best fitness: " + population.map(_._2).max + ", Worst fitness: " + population.map(_._2).min)
    val min = population.values.min
    val max = population.values.max
    val worstAcceptableOffspringFitness = min + math.max((max - min) / 4, 0.5) // too slow or too fast leads to collapse
    println("Worst acceptable fitness = " + worstAcceptableOffspringFitness)
    println("Average mutation rate = " + population.keys.asInstanceOf[Set[(Double, Any)]].map(_._1).sum / populationSize)

    // Ibrahim's supermuations when stuck...

//    def loop(generation: Map[Genome, Double]): Map[Genome, Double] = {
//      val offspring = toMap(population.keys map mutator, eval)
//      val viableOffspring = offspring.filter(_._2 >= worstAcceptableOffspringFitness)
//      val newGeneration = generation ++ viableOffspring
//      if (newGeneration.size >= populationSize) newGeneration
//      else loop(newGeneration)
//    }
//    val newGeneration = loop(population.filter(_._2 >= worstAcceptableOffspringFitness))

    val offspring = toMap(population.keys map mutator, eval)
    val everyone = population ++ offspring
//    val newGeneration = Random.shuffle(everyone.toSeq).sortBy(-_._2).take(populationSize).toMap
    val aboveCut = everyone.filter(_._2 >= worstAcceptableOffspringFitness)
    val newGeneration = if (aboveCut.size >= populationSize) aboveCut
                        else Random.shuffle(everyone.toSeq).sortBy(-_._2).take(populationSize).toMap


    val champions = newGeneration.filter(_._2 >= target)
    if (champions.nonEmpty) champions
    else {
      evolveToFitness(
        randomSubMap(populationSize, newGeneration)
      )
    }
  }

  def toMap[A, B](elements: IterableOnce[A], f: A => B): Map[A, B] = {
    val map = mutable.Map.empty[A, B]
    for (element <- elements) {
      map(element) = f(element)
    }
    map.toMap
  }

  def randomSubMap[A, B](size: Int, map: Map[A, B]): Map[A, B] = {
    require(size <= map.size)
    val shuffledKeys = Random.shuffle(map.keys.toSeq).take(size)
    toMap(shuffledKeys, map(_))
  }

  trait Target[Genome] extends (Map[Genome, Double] => Option[Target[Genome]])

  def evolveToFitness2[Genome](population: Map[Genome, Double], target: Target[Genome])
                              (implicit eval: Seq[Genome] => Map[Genome, Double],
                               mutator: Iterable[Genome] => Iterable[Genome]): Map[Genome, Double] = {
    ???
  }

  def secondLowest[A: Ordering](as: Seq[A]): A = {
    require(as.length >= 2)
    val ord = implicitly[Ordering[A]]; import ord._
    var (worst, secondWorst) = if (as(0) > as(1)) (as(1), as(0)) else (as(0), as(1))
    for (a <- as.drop(2)) {
      if (a < secondWorst) {
        if (a <= worst) {
          secondWorst = worst
          worst = a
        }
        else {
          secondWorst = a
        }
      }
    }
    secondWorst
  }

//  trait MySeq[+A] extends Iterable[A]
//    with collection.Seq[A]
//    with SeqOps[A, MySeq, MySeq[A]]
//    with IterableFactoryDefaults[A, MySeq] {
//
//    override def iterableFactory: SeqFactory[MySeq] = MySeq
//  }
//  object MySeq extends SeqFactory.Delegate[MySeq](???)

//  class Pairs[+A, +B] private(val firsts: MySeq[A], val seconds: MySeq[B]) extends Seq[(A, B)] with SeqOps[
//      (A, B),
//      ({type P2[Q] =
//        ({type P1[C, D, CD <: (C, D)] = Pairs[C, D]})#P1[_, _, Q] with Seq[Q]
//      })#P2,
//      Pairs[A, B]] {
//
//    override def filter(predicate: ((A, B)) => Boolean): this.type = {
//      ???
//    }
//
//    def map1[C](f: A => C): Pairs[C, B] = Pairs(firsts map f, seconds)
//
//    def map2[D](f: B => D): Pairs[A, D] = Pairs(firsts, seconds map f)
//
//    def pairMap[C, D](f: A => C, g: B => D): Pairs[C, D] = Pairs(firsts map f, seconds map g)
//
//    override def iterator: Iterator[(A, B)] = new Iterator[(A, B)] {
//      private val aIterator = firsts.iterator
//      private val bIterator = seconds.iterator
//      override def hasNext: Boolean = aIterator.hasNext
//      override def next(): (A, B) = (aIterator.next(), bIterator.next())
//    }
//
//    override def apply(i: Int): (A, B) = (firsts(i), seconds(i))
//
//    override def length: Int = firsts.length
//  }
//  object Pairs {
//    def apply[A, B](firsts: MySeq[A], toB: A => B): Pairs[A, B] = new Pairs(firsts, firsts map toB)
//    def apply[A, B](firsts: MySeq[A], seconds: MySeq[B]): Pairs[A, B] = {
//      require(firsts.length == seconds.length)
//      new Pairs(firsts, seconds)
//    }
//    def apply[A, B](pairs: MySeq[(A, B)]): Pairs[A, B] = new Pairs(pairs.map(_._1), pairs.map(_._2))
//  }

  type Eval[A] = A => Double
  type Eval2[A, B] = B => Eval[A]

  // Code below copy-pasted from KellyOptimizer project :S

  trait Sampleable[A] extends Iterator[A] {

    /**
      * Each call to this method returns an independent sample point from the distribution represented by this Sampleable,
      * i.e. calls to this method are i.i.d (by a slight abuse of language).
      */
    def probe(implicit random: Random = Random): A

    final override def hasNext(): Boolean = true

    final override def next(): A = probe
  }

  object Sampleable {
    def from(quantileFunction: Double => Double): Sampleable[Double] = (random: Random) => {
      quantileFunction(random.nextDouble())
    }
  }

  final case class Cauchy(center: Double, gamma: Double) extends Sampleable[Double] {

    override def probe(implicit random: Random): Double = quantile(random.nextDouble())

    def pdf(x: Double): Double = {
      val xr = x - center
      val gamma2 = gamma * gamma
      gamma2 / (math.Pi * gamma * (xr * xr + gamma2))
    }

    def cdf(x: Double): Double = math.atan((x - center) / gamma) / math.Pi + .5

    def quantile(x: Double): Double = center + gamma * math.tan(math.Pi * (x - .5))
  }
}
