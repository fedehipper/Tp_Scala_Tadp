package tadQuest

case class Heroe(HP: Double, fuerza: Double, velocidad: Double, inteligencia: Double,
                 job: Option[Trabajo] = None, inventario: Inventario = new Inventario) 
                 extends MatcheoStats(HP, fuerza, velocidad, inteligencia) { 
  
  def valorTrabajo(stat: Stat) = job.fold(0.0)(_ statJob stat) + matchStat(stat)
  
  def statFinal = inventario valorInventario(_:Stat, this) max 1
   
  def equipar(item: Item) = copy(inventario = inventario.equipar(this, item).get)
  
  def asignarTrabajo(trabajo: Trabajo) = copy(job = Some(trabajo)).actualizarEstado
      
  def desequipar(item: Item) = copy(inventario = inventario desequipar item)
   
  def statPrincipal = job.fold(None: Option[Double])(j => Some(j statPrincipal this))
  
  def modificarStats(i: IncrementoStats) = {
     copy(HP + i.HP, fuerza + i.fuerza, velocidad + i.velocidad, inteligencia + i.inteligencia)
  }
  
  def realizarTarea = (_:Tarea) afectar(this).actualizarEstado

  def actualizarEstado = copy(inventario = inventario actualizarInventario this)
}