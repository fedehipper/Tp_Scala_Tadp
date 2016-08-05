package tadQuest

case class Heroe(HP: Double, fuerza: Double, velocidad: Double, inteligencia: Double,
                 job: Option[Trabajo] = None, inventario: Inventario = new Inventario) 
                 extends MatcheoStats(HP, fuerza, velocidad, inteligencia) { 
  
  def incrementoJob(stat: Double, delta: (Trabajo, Double) => Double) = job.fold(stat)(delta(_, stat))
   
  def statJob(stat: Stat) = incrementoJob(matchStat(stat), _ baseTrabajo(stat,_))
  
  def statFinal(stat: Stat) = inventario.stat(stat)(this, statJob(stat)) max 1
    
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