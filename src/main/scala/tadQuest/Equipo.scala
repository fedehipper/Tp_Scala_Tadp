package tadQuest

case class Equipo(nombre: String, heroes: List[Heroe] = Nil, pozoComun: Double = 0) {
  
  def agregarMiembro(unMiembro: Heroe) = copy(heroes = unMiembro :: heroes) 
  def miembrosConTrabajo = heroes.filter(_.job.isDefined)
  def reemplazar(viejo: Heroe, nuevo: Heroe) = copy(heroes = nuevo :: heroes.filterNot(_ equals viejo))
  def maximo = heroes.map(_: Heroe => Double).max
  def mejorHeroeSegun(cuantificador: Heroe => Double) = heroes.find(cuantificador(_) == maximo(cuantificador))
  def incrementarPozo(cantidad: Double) = copy(pozoComun = pozoComun + cantidad)
  def incrementoStat(heroe: Heroe, item: Item) = heroe.equipar(item).statPrincipal.get - heroe.statPrincipal.get
  def equiparATodos(item: Item) = copy(heroes = heroes.map(_ equipar item))
  def cobrarRecompensa(mision: Mision): Equipo = mision.recompensa.cobrar(this)
  
  def lider: Option[Heroe] = {
    if (miembrosConTrabajo.isEmpty) None
    else copy(heroes = miembrosConTrabajo).mejorHeroeSegun(_.statPrincipal.get)
  }
 
  def incrementarStatsMiembros(condicion: Heroe => Boolean, recompensa: IncrementoStats) = {
    copy(heroes = for(heroe <- heroes if condicion(heroe)) 
      yield heroe modificarStats recompensa)
  }
  
  def obtenerItem(item: Item): Equipo = (
    for{heroe <- mejorHeroeSegun(incrementoStat(_, item))
      if incrementoStat(heroe, item) > 0}
    yield reemplazar(heroe, heroe equipar item)).getOrElse(incrementarPozo(item.precio))
   
  def elMejorPuedeRealizar(tarea: Tarea) = {
    for(facilidad <- tarea facilidadPara this; elMejor <- mejorHeroeSegun(facilidad))
    yield elMejor
  }
  
  def realizarMision(mision: Mision): Exito = 
    mision.tareas.foldLeft(Realizo(this): Exito)((resultadoAnterior, tarea) => resultadoAnterior match {
      case NoRealizo(_, _) => resultadoAnterior
      case Realizo(equipo) => postTarea(equipo, tarea).fold(NoRealizo(equipo, tarea): Exito)(Realizo(_))
    }
  ).map(_ cobrarRecompensa mision)

  def postTarea(equipo: Equipo, tarea: Tarea): Option[Equipo] = {
    for(heroe <- equipo elMejorPuedeRealizar tarea) 
    yield equipo.reemplazar(heroe, heroe realizarTarea tarea)
  }
    
  def entrenar(taberna: Taberna, criterio: (Equipo, Equipo) => Boolean): Equipo = (
    for{misionElegida <- taberna.elegirMision(criterio, this) ; equipo <- realizarMision(misionElegida).toOption}
      yield equipo entrenar(taberna.misionRealizada(misionElegida), criterio)) getOrElse(this)
      
}