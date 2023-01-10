import scala.annotation.{tailrec, unused}

package object polarizacion {

  //Tipo para los valores reales de una distribución
  type DistributionValues = Vector[Double]

  //Tipo para frecuencias de longitud k
  type Frequency = Vector[Double]

  //Tipo (Tupla) que contiene un vector de distribución y uno de frecuencias, correspondientes entre sí
  type Distribution = (Frequency, DistributionValues)

  /**
   * Función RhoER
   *
   * Recibe una distribución (Frequency, DistributionValues)
   *
   * Retorna el valor de la medida de polarización de Esteban y Ray.
   */
  def RhoEr(d: Distribution): Double = {
    val K = 10
    val k = d._2.size
    val alpha = 1.6
    val sumatoria = for{
      i <- 0 until k
      j <- 0 until k
    } yield Math.pow(d._1(i), 1 + alpha) * d._1(j) * Math.abs(d._2(i) - d._2(j))
    K * sumatoria.sum
  }

  //Tipo que contiene las creencias de b agentes sobre una proposición p
  type SpecificBeliefConf = Vector[Double]

  //Tipo con el que se definen creencias especificas de forma genérica
  @unused
  type GenericBeliefConf = Int => SpecificBeliefConf

  //Tipo que permite definir discretizaciones sobre un intérvalo
  type Discretization = Vector[Double]

  /**
   * Función Rho
   *
   * Recibe una discretización sobre el intérvalo [0, 1] y una creencia específica
   *
   * Retorna el valor de la polarización en ese conjunto de agentes
   */
  def rho(d_k: Discretization, sb: SpecificBeliefConf): Double = {
    val cd_k = 0.0 +: d_k :+ 1.0

    @tailrec
    def agentes(sbA: SpecificBeliefConf, freq: Frequency, limit: Int, means: DistributionValues): Distribution = {
      if(limit == cd_k.size) (freq, means)
      else {
        val (cumple, noCumple) = sbA.partition(_ < cd_k(limit))
        agentes(noCumple, freq :+ (cumple.length.toDouble / sb.length.toDouble), limit + 1, means :+ (cd_k(limit-1) + cd_k(limit)) / 2)
      }
    }

    RhoEr(agentes(sb, Vector(), 1, Vector()))
  }

  //Tipo que permite definir una función de influencia
  type WeightedGraph = (Int,Int) =>Double

  //Tipo que permite tener una función de influencia junto a la cantidad de agentes involucrados
  type SpecificWeightedGraph = (WeightedGraph, Int)

  //Tipo que permite definir funciones de influencia específicas de forma genérica
  @unused
  type GenericWeightedGraph = Int => SpecificWeightedGraph

  /**
   * Función showWeightedGraph
   *
   * Recibe una función de influencia específica
   *
   * Retorna una matriz asociada al grafo de influencias
   */
  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val agentes = Vector.range(0, swg._2)
    for(ag <- agentes) yield agentes.map(x => swg._1(ag, x))
  }

  /**
   * Función confBiasUpdate
   *
   * Recibe una creencia específica sb y una función de influencia específica swg
   *
   * Retorna la creencia específica después de haber aplicado la función de actualización
   */
  def confBiasUpdate(b: SpecificBeliefConf, swg: SpecificWeightedGraph): SpecificBeliefConf = {
    val agentes = Vector.range(0, swg._2)

    for {
      i <- agentes
      a = for ( j <- agentes.filter(x => swg._1(x,i) > 0) ) yield (1 - math.abs(b(j) - b(i))) * swg._1(j, i) * (b(j) - b(i))
    } yield b(i) + (a.sum / a.length)
  }

}