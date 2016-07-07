package tadQuest

import Sector._
import scala.util.Try

object NoSePudoEquiparUnItem extends Exception

case class Inventario(items: List[Item] = Nil) {
   
  def equipar(heroe: Heroe, item: Item): Try[Inventario] = Try(
    if(item cumpleCondicion heroe) {
      val equipamientoDe = item.sector match {    
        case Cabeza => equiparItem(_.sector == Cabeza) _
        case Armadura => equiparItem(_.sector == Armadura) _
        case ArmaSimple => equiparArmaSimple _
        case ArmaDoble => equiparItem(i => i.sector == ArmaSimple || i.sector == ArmaDoble) _
        case Talisman => equiparItem(i => false) _
      }
      equipamientoDe (item)
    }
    else throw NoSePudoEquiparUnItem
  )
  
  def actualizarInventario(heroe: Heroe) = copy(items.filter(_.cumpleCondicion(heroe)))
  
  def equiparItem(condicion: Item => Boolean)(item: Item) = copy(item :: items.filterNot(condicion))
  
  def equiparArmaSimple(item: Item) = {
    val armas = items.filter(_.sector == ArmaSimple)
    val armaDoble = items.filter(_.sector == ArmaDoble)
    if (armaDoble.size < 1) {
      if (armas.size < 2) equiparItem(i => false)(item)
      else copy(item :: armas.head :: items.filterNot(_.sector == ArmaSimple))
    }
    else equiparItem(_.sector == ArmaDoble)(item)
  }

  def desequipar(item: Item) = copy(items.filterNot(_ == item))
  
  def valorDeItems(i: (Item, Heroe, Double) => Double)(heroe: Heroe, valor: Double) = {
    items.foldLeft(valor)((v, item) => i(item, heroe, v))  
  }
  
  def fuerzaFinal = valorDeItems( _ fuerza(_,_)) (_, _)
  def HPFinal = valorDeItems(_ HP(_, _))(_, _)
  def velocidadFinal = valorDeItems(_ velocidad(_, _)) (_, _)
  def inteligenciaFinal = valorDeItems(_ inteligencia(_, _)) (_, _)
  
  def cantidadItems = items.size
}