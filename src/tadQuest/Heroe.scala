package tadQuest

case class Heroe(HPBase: Double, fuerzaBase: Double, velocidadBase: Double, inteligenciaBase: Double,
                 job: Option[Trabajo] = None, inventario: Inventario = new Inventario) { 
  
  def statTrabajo(base: Double, delta: (Trabajo, Double) => Double) = job.fold(base)(delta(_, base))
  
  def fuerzaFinal = mayorAUno(inventario.fuerzaFinal(statTrabajo(fuerzaBase, _ fuerza _))(this))
  def HPFinal = mayorAUno(inventario.HPFinal(statTrabajo(HPBase, _ HP _))(this))
  def velocidadFinal = mayorAUno(inventario.velocidadFinal(statTrabajo(velocidadBase,  _ velocidad _))(this))
  def inteligenciaFinal = mayorAUno(inventario.inteligenciaFinal(statTrabajo(inteligenciaBase, _ inteligencia _))(this))
  
  def mayorAUno = (_:Double) max 1 
  
  def equipar(item: Item) = copy(inventario = inventario.equipar(this, item).get)
  
  def asignarTrabajo(trabajo: Trabajo) = copy(job = Some(trabajo)).actualizarEstado
      
  def desequipar(item: Item) = copy(inventario = inventario.desequipar(item))
   
  def statPrincipal = job.fold(None: Option[Double])(t => Some(t statPrincipal this))
  
  def modificarStats(hp: Double, fuerza: Double, velocidad: Double ,inteligencia: Double) = {
     copy(HPBase + hp, fuerzaBase + fuerza, velocidadBase + velocidad, inteligenciaBase + inteligencia)
  }
  
  def realizarTarea(tarea: Tarea): Heroe = tarea.afectar(this).actualizarEstado
  
  def agregarRecompensaStats(r: StatsRecompensa): Heroe = 
    modificarStats(r.HP, r.fuerza, r.velocidad, r.inteligencia).actualizarEstado
  
  def actualizarEstado = copy(inventario = inventario.actualizarInventario(this))
   
}