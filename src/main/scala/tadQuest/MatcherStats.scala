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

trait Exito {
  def map(f:Equipo => Equipo): Exito
  def isSuccess: Boolean
  def get: Equipo
  def toOption: Option[Equipo] = if(isSuccess) Some(this.get) else None
  def isFailure: Boolean
}

case class Realizo(equipo: Equipo) extends Exito {
  def map(f: Equipo => Equipo): Exito = Realizo(f(equipo))
  def isSuccess: Boolean = true
  def get: Equipo = equipo
  def isFailure: Boolean = false
}

case class NoRealizo(equipo: Equipo, tarea: Tarea) extends Exito {
  def map(f: Equipo => Equipo): Exito = this
  def isSuccess: Boolean = false
  def get: Equipo = equipo
  def isFailure: Boolean = true
}