package tadQuest

trait Stat
case object StatFuerza extends Stat
case object StatHP extends Stat
case object StatVelocidad extends Stat
case object StatInteligencia extends Stat
  
abstract class MatcheoStats(HP: Double, fuerza: Double, velocidad: Double, inteligencia: Double) {
  def matchStat(stat: Stat) = stat match {
    case StatFuerza => fuerza
    case StatHP => HP
    case StatVelocidad => velocidad
    case StatInteligencia => inteligencia
  }
}