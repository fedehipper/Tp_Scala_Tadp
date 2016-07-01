package tadQuest

case class Heroe(HPBase: Double, fuerzaBase: Double, velocidadBase: Double, inteligenciaBase: Double,
                 job: Option[Trabajo] = None, inventario: Inventario = new Inventario) { 
  
  def statTrabajo(base: Double, delta: (Trabajo, Double) => Double) = job.foldLeft(base)((b, j) => delta(j, b))
  
  def fuerzaFinal = inventario fuerzaFinal(this, statTrabajo(fuerzaBase, _ fuerza _ )) max 1
  def HPFinal = inventario HPFinal(this, statTrabajo(HPBase, _ HP _)) max 1
  def velocidadFinal = inventario velocidadFinal(this, statTrabajo(velocidadBase,  _ velocidad _ )) max 1
  def inteligenciaFinal = inventario inteligenciaFinal(this, statTrabajo(inteligenciaBase, _ inteligencia _ )) max 1

  def equipar(item: Item): Heroe = copy(inventario = inventario.equipar(this, item).get)
  
  def asignarTrabajo(trabajo: Trabajo): Heroe = copy(job = Some(trabajo)).actualizarEstado
    
  def cantidadItems: Double = inventario.cantidadItems
  
  def desequipar(item: Item): Heroe = copy(inventario = inventario.desequipar(item))
   
  def statPrincipal: Option[Double] = {
    val semilla: Option[Double] = None
    job.foldLeft(semilla)((base, trabajo) => Some(trabajo.statPrincipal(this)))
  }
  
  def modificarStats(hp: Double, fuerza: Double, velocidad: Double ,inteligencia: Double): Heroe = {
     copy(HPBase = HPBase + hp,
          fuerzaBase = fuerzaBase + fuerza, 
          velocidadBase = velocidadBase + velocidad,
          inteligenciaBase = inteligenciaBase + inteligencia)
  }
  
  def realizarTarea(tarea: Tarea): Heroe = tarea.afectar(this).actualizarEstado
  
  def agregarRecompensaStats(r: StatsRecompensa): Heroe = 
    modificarStats(r.HP, r.fuerza, r.velocidad, r.inteligencia).actualizarEstado
  
  def actualizarEstado: Heroe = copy(inventario = inventario.actualizarInventario(this))
   
}