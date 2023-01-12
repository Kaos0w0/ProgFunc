import scala.annotation.{tailrec, unused}
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.{Map, Seq, mutable}
import common._

package object polarizacionParalela {

  //Tipo para los valores reales de una distribución
  type DistributionValuesPar = ParVector[Double]

  //Tipo para frecuencias de longitud k
  type FrequencyPar = ParVector[Double]

  //Tipo (Tupla) que contiene un vector de distribución y uno de frecuencias, correspondientes entre sí
  type DistributionPar = (FrequencyPar, DistributionValuesPar)

  /**
   * Función RhoERPar
   *
   * Recibe una distribución (FrequencyPar, DistributionValuesPar)
   *
   * Retorna el valor de la medida de polarización de Esteban y Ray.
   */
  def rhoERPar(d: DistributionPar): Double = {
    val K = 10
    val k = d._2.size
    val alpha = 1.6
    val sumatoria = for{
      i <- 0 until k
      j <- 0 until k
      (e1, e2) = parallel(Math.pow(d._1(i), 1 + alpha) * d._1(j), Math.abs(d._2(i) - d._2(j)))
    } yield e1 * e2
    K * sumatoria.sum
  }

  //Tipo que contiene las creencias de b agentes sobre una proposición p
  type SpecificBeliefConfPar = ParVector[Double]

  //Tipo con el que se definen creencias especificas de forma genérica
  @unused
  type GenericBeliefConfPar = Int => SpecificBeliefConfPar

  //Tipo que permite definir discretizaciones sobre un intérvalo
  type DiscretizationPar = ParVector[Double]

  /**
   * Función RhoPar
   *
   * Recibe una discretización sobre el intérvalo [0, 1] y una creencia específica
   *
   * Retorna el valor de la polarización en ese conjunto de agentes
   */
  def rhoPar(d_k: DiscretizationPar, sb: SpecificBeliefConfPar): Double = {
    val cd_k = 0.0 +: d_k :+ 1.0

    @tailrec
    def agentes(sbA: SpecificBeliefConfPar, freq: FrequencyPar, limit: Int, means: DistributionValuesPar): DistributionPar = {
      if(limit == cd_k.size) (freq, means)
      else {
        val (cumple, noCumple) = sbA.partition(_ < cd_k(limit))
        val (freqP, meansP) = parallel(freq :+ (cumple.length.toDouble / sb.length.toDouble), means :+ (cd_k(limit-1) + cd_k(limit)) / 2)
        agentes(noCumple, freqP, limit + 1, meansP)
      }
    }

    rhoERPar(agentes(sb, ParVector(), 1, ParVector()))
  }

  //Tipo que permite definir una función de influencia
  type WeightedGraph = (Int,Int) =>Double

  //Tipo que permite tener una función de influencia junto a la cantidad de agentes involucrados
  type SpecificWeightedGraph = (WeightedGraph, Int)
  
  /**
   * Función confBiasUpdatePar
   *
   * Recibe una creencia específica sb y una función de influencia específica swg
   *
   * Retorna la creencia específica después de haber aplicado la función de actualización
   */
  def confBiasUpdatePar(b: SpecificBeliefConfPar, swg: SpecificWeightedGraph): SpecificBeliefConfPar = {
    val agentes = ParVector.range(0, swg._2)

    for {
      i <- agentes
      a = for {
        j <- agentes.filter(x => swg._1(x,i) > 0)
        (e1, e2) = parallel(1 - math.abs(b(j) - b(i)), swg._1(j, i) * (b(j) - b(i)))
      } yield e1*e2
    } yield b(i) + (a.sum / a.length)
  }
}