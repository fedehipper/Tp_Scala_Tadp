package tadQuest

import Sector._
import scala.util.Try

object NoSePudoEquiparUnItem extends Exception

case class Inventario(items: List[Item] = Nil) {
   
  def equipar(heroe: Heroe, item: Item): Try[Inventario] = Try(
    if(item cumpleCondicion heroe) {
      val equipamientoDe = item.sector match {    
        case arma if(List(ArmaSimple, ArmaDoble).contains(arma)) => equiparArma _
        case _ => equiparSimple _
      }
      equipamientoDe(item)
    }
    else throw NoSePudoEquiparUnItem
  )
  
  def agregarItem(item: Item) = copy(item :: items)
  
  def equiparSimple(item: Item) = item.sector match{
    case Talisman => agregarItem(item)
    case _ => copy(item :: items.filterNot(_ == item.sector))
  }
  
  def equiparArma(item: Item) = {
    item.sector match {
      case ArmaSimple => {
        val armasSimples = items.filter(_.sector == ArmaSimple)
        if (items.exists(_.sector == ArmaDoble)) copy(item:: items.filterNot(_.sector == ArmaDoble))
        else {
          if (armasSimples.size < 2) agregarItem(item)
          else copy(item :: armasSimples.head :: items.filterNot(_.sector == ArmaSimple))
        }
      }
      case ArmaDoble => 
        copy(item :: items.filterNot(i => i.sector == ArmaSimple || i.sector == ArmaDoble))
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