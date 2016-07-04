package tadQuest

trait Exito {
  def map(f: Equipo => Equipo): Exito
  def toOption: Option[Equipo]
  def isSuccess: Boolean
  def get: Equipo
}

case class SuccessPrimera(equipo: Equipo) extends Exito {
  def map(f:Equipo => Equipo): Exito = SuccessPrimera(f(equipo))
  def toOption: Option[Equipo] = Some(equipo)
  def isSuccess: Boolean = true
  def get: Equipo = equipo
}

case class SuccessSegunda(equipo: Equipo) extends Exito {
  def map(f: Equipo => Equipo): Exito = SuccessSegunda(f(equipo))
  def toOption: Option[Equipo] = Some(equipo)
  def isSuccess: Boolean = true
  def get: Equipo = equipo
}

case class FallidoPor(caso: CasoFallo) extends Exito {
  def map(f: Equipo => Equipo): Exito = this
  def toOption: Option[Equipo] = None
  def isSuccess: Boolean = false
  def get: Equipo = throw new NoSuchElementException("None.get")
}

trait CasoFallo
case class PrimeraVes(equipo: Equipo) extends CasoFallo
case class TareaFallida(equipo: Equipo, tarea: Tarea) extends CasoFallo


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
  
  def incrementarPozo(cantidad: Double): Equipo = copy(pozoComun = pozoComun + cantidad)
  
  def incrementarStatsMiembros(condicion: Heroe => Boolean, recompensa: StatsRecompensa): Equipo = {
    copy(heroes = heroes.filter(condicion(_)).map(_.agregarRecompensaStats(recompensa)))
  }
 
  def incrementoStat(heroe: Heroe, item: Item): Double = heroe.equipar(item).statPrincipal.get - heroe.statPrincipal.get
  
  def obtenerItem(item: Item): Equipo = {
    val equipoConItem = for {heroe <- mejorHeroeSegun(incrementoStat(_, item))
      if incrementoStat(heroe, item) > 0
    }
    yield reemplazar(heroe, heroe equipar item)
    equipoConItem.getOrElse(incrementarPozo(item.precio))
  }
  
  def equiparATodos(item: Item): Equipo = copy(heroes = heroes.map(_.equipar(item)))
  
  def elMejorPuedeRealizar(tarea: Tarea): Option[Heroe] = {
    for {facilidad <- tarea facilidadPara this; elMejor <- mejorHeroeSegun(facilidad)}
    yield elMejor
  }
  
  def cobrarRecompensa(mision: Mision): Equipo = mision.recompensa.cobrar(this)
  
  def realizarMision(mision: Mision): Exito =
    mision.tareas.foldLeft(SuccessPrimera(this): Exito)((resultadoAnterior, tarea) => {
      resultadoAnterior match {
        case FallidoPor(cantidad) => cantidad match {
          case PrimeraVes(equipo) => SuccessSegunda(equipo)
          case TareaFallida(equipo, tareaFallida) => FallidoPor(TareaFallida(equipo, tareaFallida))
        }
        case SuccessPrimera(equipo) => 
          postTarea(equipo, tarea).fold(FallidoPor(PrimeraVes(equipo)): Exito)(SuccessPrimera(_))
        
        case SuccessSegunda(equipo) => 
          postTarea(equipo, tarea).fold(FallidoPor(TareaFallida(equipo, tarea)): Exito)(SuccessSegunda(_))
      }
  }).map(_.cobrarRecompensa(mision))

  def postTarea(equipo: Equipo, tarea: Tarea) = {
    for {heroe <- equipo elMejorPuedeRealizar tarea} 
    yield {equipo.reemplazar(heroe, heroe realizarTarea tarea)}
  }
  
  def entrenar(taberna: Taberna, criterio: (Equipo, Equipo) => Boolean): Equipo = {
    val equipo = this
    val resultadoEntrenar = for {
      misionElegida <- taberna.elegirMision(criterio, this)
      equipo <- realizarMision(misionElegida).toOption
    }
    yield equipo.entrenar(taberna misionRealizada misionElegida, criterio)
    resultadoEntrenar.getOrElse(equipo)
  }
 
}