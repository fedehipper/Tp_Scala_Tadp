package tadQuest

import Sector._
import scala.util.Try

object NoSePudoEquiparUnItem extends Exception

case class Inventario(items: List[Item] = Nil) {
   
  def equipar(heroe: Heroe, item: Item): Try[Inventario] = Try (
    if(item cumpleCondicion heroe) {
      val equipamientoDe = item.sector match {    
        case ArmaDoble => equiparArmaDoble _
        case ArmaSimple => equiparArmaSimple _
        case _ => equiparUnicoItem _
      }
      equipamientoDe(item)
    }
    else throw NoSePudoEquiparUnItem
  )
  
  def agregarItem(item: Item) = copy(item :: items)
  
  def equiparUnicoItem(item: Item) = item.sector match{
    case Talisman => agregarItem(item)
    case _ => copy(item :: items.filterNot(_ == item.sector))
  }
  
  def equiparArmaDoble(item: Item) = {
    copy(item :: items.filterNot(i => i.sector == ArmaSimple || i.sector == ArmaDoble))
  }
  
  def equiparArmaSimple(item: Item) = {
    val armasSimples = items.filter(_.sector == ArmaSimple)
    if (items.exists(_.sector == ArmaDoble)) copy(item:: items.filterNot(_.sector == ArmaDoble))
    else {
      if (armasSimples.size < 2 && armasSimples.isEmpty) agregarItem(item)
      else copy(item :: armasSimples.head :: items.filterNot(_.sector == ArmaSimple))
    }
  }
  
  def desequipar(item: Item) = copy(items.filterNot(_ == item))
  
  def valorDeItems(i: (Item, Heroe, Double) => Double)(heroe: Heroe, valor: Double) = {
    items.foldLeft(valor)((v, item) => i(item, heroe, v))
  }
  
  def stat(statFinal: StatFinal): (Heroe, Double) => Double = {
    statFinal match {
      case FuerzaFinal => valorDeItems(_ fuerza(_, _)) _
      case HPFinal => valorDeItems(_ HP(_, _)) _
      case VelocidadFinal => valorDeItems(_ velocidad(_, _)) _
      case InteligenciaFinal => valorDeItems(_ inteligencia(_, _)) _
    }
  }
 
  def cantidadItems = items.size
  
  def actualizarInventario(heroe: Heroe) = copy(items.filter(_.cumpleCondicion(heroe)))
  
}