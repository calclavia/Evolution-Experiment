package com.calclavia.evolution

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Random

/**
 * An experimental project to test evolution of strings.
 * @author Calclavia
 */
object Evolution
{
  val random = new Random()

  var targetString = "randomlongword"
  var population = new Array[DNA](500)

  def main(args: Array[String])
  {
    println("Type in a word to evolve to:")
    targetString = StdIn.readLine().replaceAll("[^A-Za-z0-9]", "").toLowerCase

    println("Evolution aim: " + targetString)

    Thread.sleep(1000)

    println("Generating a population of: " + population.size)
    population = population map (x => new DNA)

    Thread.sleep(1000)

    var generation = 0
    var success = false

    while (!success)
    {
      //Build mating pool
      val matingPool = ArrayBuffer.empty[DNA]

      population.foreach(
        dna =>
        {
          (0 until (dna.getFitness * 100).toInt) foreach (_ => matingPool += dna)
        }
      )

      (0 until population.size).foreach(
        i =>
        {
          //Reproduce and replace old population
          val parentA = matingPool(random.nextInt(matingPool.size))
          val parentB = matingPool(random.nextInt(matingPool.size))
          //TODO: Check they are unique parents

          val child = parentA.geneticCrossover(parentB).mutate()
          //  println((child.getFitness * 100).toInt + " : " + child)

          if (child.getFitness == 1)
            success = true

          population(i) = child
        }
      )

      val best = population.sortWith(_.getFitness > _.getFitness).head
      println("Generation " + generation + " is " + (best.getFitness * 100).toInt + "% correct : " + best)

      generation += 1
      Thread.sleep(50)
    }

    println("Perfect species found with " + generation + " generations!")
  }

  def randomCharacter = (Evolution.random.nextInt(26) + 97).toChar
}
