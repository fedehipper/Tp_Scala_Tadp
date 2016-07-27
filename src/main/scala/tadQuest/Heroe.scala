package tadQuest

trait StatFinal
case object FuerzaFinal extends StatFinal
case object HPFinal extends StatFinal
case object VelocidadFinal extends StatFinal
case object InteligenciaFinal extends StatFinal


case class Heroe(HP: Double, fuerza: Double, velocidad: Double, inteligencia: Double,
                 job: Option[Trabajo] = None, inventario: Inventario = new Inventario) { 
  
  def statTrabajo(stat: Double, incremento: (Trabajo, Double) => Double) = job.fold(stat)(incremento(_, stat))

  def stat(statFinal: StatFinal) = {
    val valorParcial = inventario.stat(statFinal)
    (statFinal match {
      case FuerzaFinal => valorParcial(this, statTrabajo(fuerza, _ fuerza _))
      case HPFinal => valorParcial(this ,statTrabajo(HP, _ HP _))
      case VelocidadFinal => valorParcial(this, statTrabajo(velocidad,  _ velocidad _)) 
      case InteligenciaFinal => valorParcial(this, statTrabajo(inteligencia, _ inteligencia _))
    }) max 1 
  }
 
  def equipar(item: Item) = copy(inventario = inventario.equipar(this, item).get)
  
  def asignarTrabajo(trabajo: Trabajo) = copy(job = Some(trabajo)).actualizarEstado
      
  def desequipar(item: Item) = copy(inventario = inventario.desequipar(item))
   
  def statPrincipal = job.fold(None: Option[Double])(t => Some(t statPrincipal this))
  
  def modificarStats(i: IncrementoStats) = {
     copy(HP + i.HP, fuerza + i.fuerza, velocidad + i.velocidad, inteligencia + i.inteligencia)
  }
  
  def realizarTarea(tarea: Tarea): Heroe = tarea.afectar(this).actualizarEstado

  def actualizarEstado = copy(inventario = inventario.actualizarInventario(this))
}