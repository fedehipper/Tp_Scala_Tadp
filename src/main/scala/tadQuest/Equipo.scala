package tadQuest

trait Exito {
  def map(f:Equipo => Equipo): Exito
  def isSuccess: Boolean
  def get: Equipo
  def toOption: Option[Equipo] = if(isSuccess) Some(this.get) else None
  def isFailure: Boolean
}

case class PudoRealizar(equipo: Equipo) extends Exito {
  def map(f: Equipo => Equipo): Exito = PudoRealizar(f(equipo))
  def isSuccess: Boolean = true
  def get: Equipo = equipo
  def isFailure: Boolean = false
}

case class NoPudoRealizar(equipo: Equipo, tarea: Tarea) extends Exito {
  def map(f: Equipo => Equipo): Exito = this
  def isSuccess: Boolean = false
  def get: Equipo = equipo
  def isFailure: Boolean = true
}

case class Equipo(nombre: String, heroes: List[Heroe] = Nil, pozoComun: Double = 0) {
  
  def agregarMiembro(unMiembro: Heroe) = copy(heroes = unMiembro :: heroes) 
  def miembrosConTrabajo = heroes.filter(_.job.isDefined)
  def reemplazar(viejo: Heroe, nuevo: Heroe) = copy(heroes = nuevo :: heroes.filterNot(_ equals viejo))
  
  def lider: Option[Heroe] = {
    if (miembrosConTrabajo.isEmpty) None
    else copy(heroes = miembrosConTrabajo).mejorHeroeSegun(_.statPrincipal.get)
  }
 
  def maximo = heroes.map(_: Heroe => Double).max
  
  def mejorHeroeSegun(cuantificador: Heroe => Double) = heroes.find(cuantificador(_) equals maximo(cuantificador))
  
  def incrementarPozo(cantidad: Double) = copy(pozoComun = pozoComun + cantidad)
  
  def incrementarStatsMiembros(condicion: Heroe => Boolean, recompensa: IncrementoStats) = {
    copy(heroes = heroes filter(condicion(_)) map(_ modificarStats recompensa))
  }
 
  def incrementoStat(heroe: Heroe, item: Item) = heroe.equipar(item).statPrincipal.get - heroe.statPrincipal.get
  
  def obtenerItem(item: Item): Equipo = (
    for {heroe <- mejorHeroeSegun(incrementoStat(_, item))
      if incrementoStat(heroe, item) > 0}
    yield reemplazar(heroe, heroe equipar item)
  ).getOrElse(incrementarPozo(item.precio))
  
  def equiparATodos(item: Item) = copy(heroes = heroes.map(_ equipar item))
  
  def elMejorPuedeRealizar(tarea: Tarea) = {
    for(facilidad <- tarea facilidadPara this; elMejor <- mejorHeroeSegun(facilidad))
    yield elMejor
  }
  
  def cobrarRecompensa(mision: Mision): Equipo = mision.recompensa.cobrar(this)
  
  def realizarMision(mision: Mision): Exito = 
    mision.tareas.foldLeft(PudoRealizar(this): Exito)((resultadoAnterior, tarea) => resultadoAnterior match {
      case NoPudoRealizar(_, _) => resultadoAnterior
      case PudoRealizar(equipo) => postTarea(equipo, tarea).fold(NoPudoRealizar(equipo, tarea): Exito)(PudoRealizar(_))
    }
  ).map(_ cobrarRecompensa mision)

  def postTarea(equipo: Equipo, tarea: Tarea): Option[Equipo] = {
    for(heroe <- equipo elMejorPuedeRealizar tarea) 
    yield equipo.reemplazar(heroe, heroe realizarTarea tarea)
  }
    
  def entrenar(taberna: Taberna, criterio: (Equipo, Equipo) => Boolean): Equipo = {
    val equipo = this
    (for {
      misionElegida <- taberna.elegirMision(criterio, this)
      equipo <- realizarMision(misionElegida).toOption
    }
    yield equipo.entrenar(taberna misionRealizada misionElegida, criterio)).getOrElse(equipo)
  }
 
}