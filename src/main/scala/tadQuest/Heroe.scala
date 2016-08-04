package tadQuest

trait Stat
case object StatFuerza extends Stat
case object StatHP extends Stat
case object StatVelocidad extends Stat
case object StatInteligencia extends Stat

case class Heroe(HP: Double, fuerza: Double, velocidad: Double, inteligencia: Double,
                 job: Option[Trabajo] = None, inventario: Inventario = new Inventario) { 
  
  def statTrabajo(stat: Double, incremento: (Trabajo, Double) => Double) = job.fold(stat)(incremento(_, stat))
  
  def stat(statFinal: Stat) = {
    inventario.stat(statFinal)(this, statFinal match {
      case StatFuerza => statTrabajo(fuerza, _ fuerza _)
      case StatHP => statTrabajo(HP, _ HP _)
      case StatVelocidad => statTrabajo(velocidad,  _ velocidad _)
      case StatInteligencia => statTrabajo(inteligencia, _ inteligencia _)
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