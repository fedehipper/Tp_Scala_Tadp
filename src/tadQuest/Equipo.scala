package tadQuest
import scala.util.{Try, Failure, Success}

abstract class TareaFallida extends Exception
case class TareaFallidaUno(equipo: Equipo) extends TareaFallida 
case class TareaFallidaDos(equipo: Equipo, tarea: Tarea) extends TareaFallida
 
trait Exito {
  def map(f: Equipo => Equipo): Exito
  def toOption: Option[Equipo]
  def isSuccess: Boolean = true
  def get: Equipo
  def isFailure: Boolean
}

case class SuccessConFallida(equipo: Equipo) extends Exito {
  def map(f: Equipo => Equipo) = SuccessConFallida(f(equipo))
  def toOption: Option[Equipo] = Some(equipo)
  def get: Equipo = equipo
  def isFailure: Boolean = false
}
case class SuccessSinFallida(equipo: Equipo) extends Exito {
  def map(f: Equipo => Equipo) = SuccessSinFallida(f(equipo))
  def toOption: Option[Equipo] = Some(equipo)
  def get: Equipo = equipo
  def isFailure: Boolean = false
}
case class Fallo(tareaFallida: TareaFallida) extends Exito {
  def map(f: Equipo => Equipo) = this
  override def toOption: Option[Equipo] = None
  override def isSuccess: Boolean = false
  def get = throw new Exception(tareaFallida)
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
  
  def mejorHeroeSegun(cuantificador: Heroe => Double) = heroes.find(cuantificador(_) == maximo(cuantificador))
  
  def incrementarPozo(cantidad: Double) = copy(pozoComun = pozoComun + cantidad)
  
  def incrementarStatsMiembros(condicion: Heroe => Boolean, recompensa: IncrementoStats) = {
    copy(heroes = heroes.filter(condicion(_)).map(_.modificarStats(recompensa)))
  }
 
  def incrementoStat(heroe: Heroe, item: Item) = heroe.equipar(item).statPrincipal.get - heroe.statPrincipal.get
  
  def obtenerItem(item: Item): Equipo = {
    val equipoConItem = for {heroe <- mejorHeroeSegun(incrementoStat(_, item))
      if incrementoStat(heroe, item) > 0
    }
    yield reemplazar(heroe, heroe equipar item)
    equipoConItem.getOrElse(incrementarPozo(item.precio))
  }
  
  def equiparATodos(item: Item) = copy(heroes = heroes.map(_.equipar(item)))
  
  def elMejorPuedeRealizar(tarea: Tarea): Option[Heroe] = {
    for {facilidad <- tarea facilidadPara this; elMejor <- mejorHeroeSegun(facilidad)}
    yield elMejor
  }
  
  def cobrarRecompensa(mision: Mision): Equipo = mision.recompensa.cobrar(this)
  
  def realizarMision(mision: Mision): Exito = { 
    val resultadoRealizar: Exito =  mision.tareas.foldLeft(SuccessSinFallida(this): Exito)((resultadoAnterior, tarea) => {
      resultadoAnterior match {
        case Fallo(tareaFallida) => tareaFallida match {
          case TareaFallidaDos(equipo, tarea) => Fallo(TareaFallidaDos(equipo, tarea)) 
          case TareaFallidaUno(equipo) => SuccessConFallida(equipo)
        }
        case SuccessConFallida(equipo) =>
          postTarea(equipo, tarea).fold(Fallo(TareaFallidaDos(equipo, tarea)): Exito)(SuccessConFallida(_))
        case SuccessSinFallida(equipo) => 
          postTarea(equipo, tarea).fold(Fallo(TareaFallidaUno(equipo)): Exito)(SuccessSinFallida(_))
      }
    }) 
    resultadoRealizar match {
      case Fallo(tareaFallida) => Fallo(tareaFallida)
      case SuccessConFallida(equipo) => SuccessConFallida(equipo)
      case SuccessSinFallida(equipo) => SuccessSinFallida(equipo).map(_ cobrarRecompensa(mision))    
    }
  }
 
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