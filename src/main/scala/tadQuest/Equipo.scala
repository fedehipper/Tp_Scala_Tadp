package tadQuest

case class Equipo(nombre: String, heroes: List[Heroe] = Nil, pozoComun: Double = 0) {
  
  def agregarMiembro(unMiembro: Heroe) = copy(heroes = unMiembro :: heroes) 
  def miembrosConTrabajo = heroes.filter(_.job.isDefined)
  def reemplazar(viejo: Heroe, nuevo: Heroe) = copy(heroes = nuevo :: heroes.filterNot(_ == viejo))
  
  def lider: Option[Heroe] = {
    if (miembrosConTrabajo.nonEmpty) copy(heroes = miembrosConTrabajo).mejorHeroeSegun(_.statPrincipal.get)
    else None
  }
 
  def maximo = heroes.map(_: Heroe => Double).max
  
  def mejorHeroeSegun(cuantificador: Heroe => Double) = heroes.find(cuantificador(_) == maximo(cuantificador))
  
  def incrementarPozo(cantidad: Double) = copy(pozoComun = pozoComun + cantidad)
   
  def incrementarStatsMiembros(condicion: Heroe => Boolean, recompensa: IncrementoStats) = {
    copy(heroes = for(heroe <- heroes if condicion(heroe)) yield heroe modificarStats recompensa)
  }
  
  def incrementoStat(heroe: Heroe, item: Item) = heroe.equipar(item).statPrincipal.get - heroe.statPrincipal.get
  
  def obtenerItem(item: Item): Equipo = (
    for(heroe <- mejorHeroeSegun(incrementoStat(_, item))
      if incrementoStat(heroe, item) > 0)
    yield reemplazar(heroe, heroe equipar item)).getOrElse(incrementarPozo(item.precio))
  
  def equiparATodos(item: Item) = copy(heroes = heroes.map(_.equipar(item)))
  
  def elMejorPuedeRealizar(tarea: Tarea): Option[Heroe] = {
    for(facilidad <- tarea facilidadPara this; elMejor <- mejorHeroeSegun(facilidad))
    yield elMejor
  }
  
  def cobrarRecompensa(mision: Mision): Equipo = mision.recompensa.cobrar(this)
  
  def realizarMision(mision: Mision): ResultadoMision = 
    mision.tareas.foldLeft(CumpleMision(this): ResultadoMision)((anterior, tarea) => anterior match {
      case FallaMision(_,_) => anterior
      case _ =>
        postTarea(anterior.get, tarea).fold(anterior.falloTarea(tarea))(equipo => anterior.map(_ => equipo))
    }
  ).terminar(mision)

  def postTarea(equipo: Equipo, tarea: Tarea) = {
    for(heroe <- equipo elMejorPuedeRealizar tarea) 
    yield equipo.reemplazar(heroe, heroe realizarTarea tarea)
  }
  
  def entrenar(taberna: Taberna, criterio: (Equipo, Equipo) => Boolean): Equipo = {
    (for{misionElegida <- taberna.elegirMision(criterio, this)
         equipo <- realizarMision(misionElegida).toOption}
    yield equipo.entrenar(taberna misionRealizada misionElegida, criterio)).getOrElse(this)
  }
  
}