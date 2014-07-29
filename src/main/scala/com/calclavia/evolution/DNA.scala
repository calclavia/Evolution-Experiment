package com.calclavia.evolution

/**
 * @author Calclavia
 */
class DNA
{
  //A list of characters that represents a string
  val genes = ((0 until Evolution.targetString.size) map (x => Evolution.randomCharacter)).toArray
  val mutateRate = 0.1

  /**
   * The fitness of this DNA from a scale of 0 to 1.
   */
  def getFitness: Double =
  {
    return ((0 until genes.size) count (i => genes(i) == Evolution.targetString(i))) / genes.size.toDouble
  }

  def geneticCrossover(partner: DNA): DNA =
  {
    val child = new DNA

    (0 until child.genes.size).foreach(
      i =>
      {
        if (Evolution.random.nextFloat() > 0.5)
          child.genes(i) = genes(i)
        else
          child.genes(i) = partner.genes(i)
      }
    )

    return child
  }

  def mutate(): DNA =
  {
    (0 until genes.size) filter (i => Evolution.random.nextFloat() < mutateRate) foreach (i => genes(i) = Evolution.randomCharacter)
    return this
  }

  override def toString = new String(genes)
}
