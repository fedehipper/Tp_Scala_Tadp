package tadQuest

abstract class Trabajo(val tHP: Double = 0, val tFuerza: Double = 0,
                       val tVelocidad: Double = 0, val tInteligencia: Double = 0)
                       extends MatcheoStats(tHP, tFuerza, tVelocidad, tInteligencia) {

  def baseTrabajo(stat: Stat, base: Double) = matchStat(stat) + base
  def statPrincipal(heroe: Heroe): Double
}

object Guerrero extends Trabajo(tHP = 10, tFuerza = 15, tInteligencia = -10) {
  def statPrincipal(heroe: Heroe) = heroe statFinal StatFuerza
}

object Mago extends Trabajo(tInteligencia = 20, tFuerza = -20) {
  def statPrincipal(heroe: Heroe) = heroe statFinal StatInteligencia
}

object Ladron extends Trabajo(tVelocidad = 10, tHP = -5) {
  def statPrincipal(heroe: Heroe) = heroe statFinal StatVelocidad
}
