package tadQuest

abstract class Trabajo(val tHP: Double = 0, val tFuerza: Double = 0, 
    val tVelocidad: Double = 0, val tInteligencia: Double = 0)
    extends MatcherStats(tHP, tVelocidad, tFuerza, tInteligencia){
  
  def statJob(stat: Stat, base: Double) = matcheoStats(stat) + base
  def statPrincipal(heroe: Heroe): Double
}

case object Guerrero extends Trabajo(tHP = 10, tFuerza = 15, tInteligencia = -10) {
  def statPrincipal(heroe: Heroe) = heroe.stat(StatFuerza)
}
case object Mago extends Trabajo(tFuerza = -20, tInteligencia = 20) {
  def statPrincipal(heroe: Heroe) = heroe.stat(StatInteligencia)
}
case object Ladron extends Trabajo (tHP = -5, tVelocidad = 10) {
  def statPrincipal(heroe: Heroe) = heroe.stat(StatVelocidad)
}