package tadQuest

abstract class Trabajo(val tHP: Double = 0, val tFuerza: Double = 0,
                       val tVelocidad: Double = 0, val tInteligencia: Double = 0) {
 
  def HP(base: Double) = base + tHP
  def velocidad(base: Double) = base + tVelocidad
  def fuerza(base: Double) = base + tFuerza
  def inteligencia(base: Double) = base + tInteligencia
 
  def statPrincipal(heroe: Heroe): Double
}

object Guerrero extends Trabajo(tHP = 10, tFuerza = 15, tInteligencia = -10) {
  def statPrincipal(heroe: Heroe) = heroe stat FuerzaFinal
}

object Mago extends Trabajo(tInteligencia = 20, tFuerza = -20) {
  def statPrincipal(heroe: Heroe) = heroe stat InteligenciaFinal
}

object Ladron extends Trabajo(tVelocidad = 10, tHP = -5) {
  def statPrincipal(heroe: Heroe) = heroe stat VelocidadFinal
}
