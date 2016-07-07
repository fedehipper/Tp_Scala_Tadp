package tadQuest

import Sector._
import scala.util.Try

object NoSePudoEquiparUnItem extends Exception

case class Inventario(items: List[Item] = Nil) {
   
  def equipar(heroe: Heroe, item: Item): Try[Inventario] = Try(
    if(item cumpleCondicion heroe) {
      val equipamientoDe = item.sector match {    
        case ArmaSimple => equiparArmaSimple _
        case ArmaDoble => equiparArmaDoble _
        case _ => equiparSimple _
      }
      equipamientoDe(item)
    }
    else throw NoSePudoEquiparUnItem
  )
  
  def equiparSimple(item: Item) = item.sector match{
    case Talisman => copy(item :: items)
    case _ => copy(item :: items.filterNot( _ == item.sector))
  }
  
  def equiparArmaDoble(item: Item) = copy(item :: items.filterNot(i => i.sector == ArmaSimple || i.sector == ArmaDoble))
  
  def equiparArmaSimple(item: Item) = {
    val armasSimples = items.filter(_.sector == ArmaSimple)
    if (items.exists(_.sector == ArmaDoble)) copy(item:: items.filterNot(_.sector == ArmaDoble))
    else {
      if (armasSimples.size < 2) copy(item :: items)
      else copy(item :: armasSimples.head :: items.filterNot(_.sector == ArmaSimple))
    }
  }
  

  def desequipar(item: Item) = copy(items.filterNot(_ == item))
  
  def valorDeItems(i: (Item, Heroe, Double) => Double)(heroe: Heroe, valor: Double) = {
    items.foldLeft(valor)((v, item) => i(item, heroe, v))  
  }
  
  def fuerzaFinal = valorDeItems( _ fuerza(_,_))(_, _)
  def HPFinal = valorDeItems(_ HP(_, _))(_, _)
  def velocidadFinal = valorDeItems(_ velocidad(_, _))(_, _)
  def inteligenciaFinal = valorDeItems(_ inteligencia(_, _))(_, _)
  
  def cantidadItems = items.size
  
  def actualizarInventario(heroe: Heroe) = copy(items.filter(_.cumpleCondicion(heroe)))
  
}