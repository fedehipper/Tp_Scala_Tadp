package tadQuest

trait Recompensa {
  def cobrar: Equipo => Equipo
}

case class GanarOroParaElPozoComun(oro: Double) extends Recompensa {
  def cobrar = _ incrementarPozo oro 
}

case class EncontrarUnItem(item: Item) extends Recompensa {
  def cobrar = _ obtenerItem item
}

case class IncrementarStats(condicion: Heroe => Boolean, recompensa: IncrementoStats) extends Recompensa {
  def cobrar = _ incrementarStatsMiembros(condicion, recompensa)
}

case class EncontrarNuevoMiembro(heroe: Heroe) extends Recompensa {  
  def cobrar = _ agregarMiembro heroe
}

case class IncrementoStats(HP: Double = 0, fuerza: Double = 0, velocidad: Double = 0, inteligencia: Double = 0)

class Mision(val tareas: List[Tarea], val recompensa: Recompensa) {
  def facilidad(equipo: Equipo) = (
    for {tarea <- tareas; heroe <- equipo.elMejorPuedeRealizar(tarea)
      if equipo.elMejorPuedeRealizar(tarea).isDefined}
    yield tarea.facilidadPara(equipo).get (heroe)).sum
}