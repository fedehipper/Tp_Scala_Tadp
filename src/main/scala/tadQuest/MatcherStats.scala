package tadQuest

trait Stat
case object StatVelocidad extends Stat
case object StatInteligencia extends Stat
case object StatFuerza extends Stat
case object StatHP extends Stat

abstract class MatcherStats(HP: Double, velocidad: Double, fuerza: Double, inteligencia: Double) {
  def matcheoStats(stat: Stat) = stat match {
    case StatVelocidad => velocidad
    case StatFuerza => fuerza
    case StatHP => HP
    case StatInteligencia => inteligencia
  }  
}