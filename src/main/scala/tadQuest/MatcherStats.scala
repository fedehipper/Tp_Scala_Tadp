package tadQuest

trait Stat
case object StatVelocidad extends Stat
case object StatInteligencia extends Stat
case object StatFuerza extends Stat
case object StatHP extends Stat

abstract class MatcherStats(HP: Double, velocidad: Double, fuerza: Double, inteligencia: Double) {
  def matchStat(stat: Stat) = stat match {
    case StatVelocidad => velocidad
    case StatFuerza => fuerza
    case StatHP => HP
    case StatInteligencia => inteligencia
  }  
}

case class TareaFallida(equipo: Equipo, tarea: Tarea) extends Exception

trait ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision
  def falloTarea(tarea: Tarea): ResultadoMision
  def terminar(mision: Mision): ResultadoMision
  def isSuccess: Boolean = true
  def toOption: Option[Equipo] = if(isSuccess) Some(this.get) else None
  def get: Equipo
}
case class CumpleMision(equipo: Equipo) extends ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision = CumpleMision(f(equipo))
  def falloTarea(tarea: Tarea): ResultadoMision = CumpleMisionParcial(equipo)
  def terminar(mision: Mision): ResultadoMision = this.map(_.cobrarRecompensa(mision))
  def get = equipo
}
case class CumpleMisionParcial(equipo: Equipo) extends ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision = CumpleMisionParcial(f(equipo))
  def falloTarea(tarea: Tarea): ResultadoMision = FallaMision(equipo,tarea)
  def terminar(mision: Mision): ResultadoMision = this
  def get = equipo
}
case class FallaMision(equipo: Equipo, tarea: Tarea) extends ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision = this
  def falloTarea(tarea: Tarea): ResultadoMision = this
  def terminar(mision: Mision): ResultadoMision = this
  override def isSuccess: Boolean = false
  def get = equipo
}
