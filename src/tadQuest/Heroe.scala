package tadQuest

case class Heroe(HPBase: Double, fuerzaBase: Double, velocidadBase: Double, inteligenciaBase: Double,
                 job: Option[Trabajo] = None, inventario: Inventario = new Inventario) { 
  
  def statTrabajo(base: Double, incremento: (Trabajo, Double) => Double) = job.fold(base)(incremento(_, base))
  
  def fuerzaFinal = inventario.fuerzaFinal(this, statTrabajo(fuerzaBase, _ fuerza _)) max 1
  def HPFinal = inventario.HPFinal(this, statTrabajo(HPBase, _ HP _)) max 1
  def velocidadFinal = inventario.velocidadFinal(this, statTrabajo(velocidadBase,  _ velocidad _)) max 1
  def inteligenciaFinal = inventario.inteligenciaFinal(this, statTrabajo(inteligenciaBase, _ inteligencia _)) max 1
  
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