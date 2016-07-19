package tadQuest
import scala.util.{Try, Failure, Success}

case class TareaFallida(equipo: Equipo, tarea: Tarea) extends Exception
 

trait ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision
  def falloTarea(tarea: Tarea): ResultadoMision
  def terminar(mision: Mision): ResultadoMision
  def isSuccess: Boolean = true
  def toOption: Option[Equipo] = if(isSuccess) Some(this.get) else None
  def get : Equipo
}
case class CumpleMision(equipo: Equipo) extends ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision = CumpleMision(f(equipo))
  def falloTarea(tarea: Tarea): ResultadoMision = CumpleMisionParcial(equipo)
  def terminar(mision: Mision): ResultadoMision = this.map(_.cobrarRecompensa(mision))
  def get = equipo
}
case class CumpleMisionParcial(equipo: Equipo) extends ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision = CumpleMisionParcial(f(equipo))
  def falloTarea(tarea: Tarea): ResultadoMision = FallaMision(equipo,tarea)
  def terminar(mision: Mision): ResultadoMision = this
  def get = equipo
}
case class FallaMision(equipo: Equipo, tarea: Tarea) extends ResultadoMision {
  def map(f: Equipo => Equipo): ResultadoMision = this
  def falloTarea(tarea: Tarea): ResultadoMision = this
  def terminar(mision: Mision): ResultadoMision = this
  override def isSuccess: Boolean = false
  def get = equipo
}


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
    copy(heroes = heroes.filter(condicion(_)).map(_.modificarStats(recompensa)))
  }
 
  def incrementoStat(heroe: Heroe, item: Item) = heroe.equipar(item).statPrincipal.get - heroe.statPrincipal.get
  
  def obtenerItem(item: Item): Equipo = {
    val darAlMejor = for(heroe <- mejorHeroeSegun(incrementoStat(_, item))
      if incrementoStat(heroe, item) > 0)
    yield reemplazar(heroe, heroe equipar item)
    darAlMejor.getOrElse(incrementarPozo(item.precio))
  }
  
  def equiparATodos(item: Item) = copy(heroes = heroes.map(_.equipar(item)))
  
  def elMejorPuedeRealizar(tarea: Tarea): Option[Heroe] = {
    for {facilidad <- tarea facilidadPara this; elMejor <- mejorHeroeSegun(facilidad)}
    yield elMejor
  }
  
  def cobrarRecompensa(mision: Mision): Equipo = mision.recompensa.cobrar(this)
  
  def realizarMision(mision: Mision): ResultadoMision = 
    mision.tareas.foldLeft(CumpleMision(this): ResultadoMision)((anterior, tarea) => {
      anterior match {
        case FallaMision(_,_) => anterior
        case _ => postTarea(anterior.get, tarea).fold(anterior.falloTarea(tarea))(equipo => anterior.map(_ => equipo))
      }
    }
  ).terminar(mision)

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
  
  def elegirItem(items: List[Item], criterio: (Equipo, Equipo) => Boolean, mision: Mision): Item = {
    items.foldLeft(items.head)((unItem, otroItem) => {
      var estado1 = obtenerItem(unItem).realizarMision(mision)
      var estado2 = obtenerItem(otroItem).realizarMision(mision)
     (estado1, estado2) match {
        case (FallaMision(_, _), FallaMision(_, _)) => otroItem  
        case (FallaMision(_, _), _) => otroItem
        case (_, FallaMision(_, _)) => unItem  
        case _ => if(criterio(estado1.get, estado2.get)) unItem else otroItem
      }
    })
  }
  
 
}