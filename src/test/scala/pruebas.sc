import polarizacion._
import polarizacionParalela._
import scala.collection.parallel.CollectionConverters._

val pi1 = Vector(0.4 , 0.6)
val pi2 = Vector(0.5 , 0.5)
val pi3 = Vector(0.6 , 0.4)
val pi4 = Vector(0.1 , 0.9)
val pi5 = Vector(0.9 , 0.1)
val y = Vector(1.0, 5.0)
val yp = y.par

// (vP: Vector de pruebas, f1: función 1)
val vPf1 = Vector(pi1, pi2, pi3, pi4, pi5)

println(for{
  i <- 0 to 4
} yield(rhoER(vPf1(i), y), rhoERPar(vPf1(i).par, yp)))

val d1 = Vector(0.2,0.4,0.6,0.8)
val d2 = Vector(0.1,0.4,0.7,0.9)

def b1(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

def b2(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int)=> if (i <= nags/2) 0.3 else 0.9)
}

def b3(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int) => (i+1).toDouble/nags.toDouble)
}

val b1_10= b1(10)
val b2_10= b2(10)
val b3_10= b3(10)

//Vector de bx_x
val vDx = Vector(d1, d2)
val vBx = Vector(b1_10, b2_10, b3_10)

println(for{
  dx <- vDx
  vBx <- vBx
} yield (rho(dx, vBx), rhoPar(dx.par, vBx.par)) )

def i1(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) => if (i == j) 1.0
  else if (i < j) 1.0 / (j - i).toDouble
  else 0.0, nags)
}

def i2(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) => if (i == j) 1.0
  else if (i < j) (j - i).toDouble / nags.toDouble
  else (nags - (i - j)).toDouble / nags.toDouble, nags)
}

val i1_10 = i1(10)
val i2_10 = i2(10)

val vIx = Vector(i1_10, i2_10)

println(showWeightedGraph(i1_10))
println(showWeightedGraph(i2_10))

println(for{
  vBx <- vBx
  vIx <- vIx
} yield (confBiasUpdate(vBx, vIx), confBiasUpdatePar(vBx.par, vIx)) )

for {
  b <- simulate(confBiasUpdate, i1_10, b1_10, 10)
} yield (b, rho(d1, b))