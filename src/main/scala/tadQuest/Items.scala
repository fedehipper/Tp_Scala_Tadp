package tadQuest


object Sector extends Enumeration {
  type Sector = Value
  val Cabeza, Armadura, ArmaSimple, ArmaDoble, Talisman = Value
}
import Sector._

abstract class Item(val sector: Sector, val iFuerza: Double = 0, val iVelocidad: Double = 0,
    val iInteligencia: Double = 0, val iHP: Double = 0, val precio: Double = 0) {
  
  def HP(heroe: Heroe, valor: Double) = iHP + valor
  def velocidad(heroe: Heroe, valor: Double) = iVelocidad + valor
  def inteligencia(hereo: Heroe, valor: Double) = iInteligencia + valor
  def fuerza(hereo: Heroe, valor: Double) = iFuerza + valor
  
  def cumpleCondicion(heroe: Heroe): Boolean = true
}

object CascoVikingo extends Item(Cabeza, iHP = 10, precio = 5) {
  override def cumpleCondicion(heroe: Heroe) = heroe.fuerza > 30
}

object PalitoMagico extends Item(ArmaSimple, iInteligencia = 20, precio = 25) {
  override def cumpleCondicion(heroe: Heroe) = {
    val condicion = for {trabajo <- heroe.job
      if(trabajo == Mago || (trabajo == Ladron && heroe.inteligencia > 30))}
    yield heroe
    condicion.isDefined
  }  
}

object ArmaduraEleganteSport extends Item(Armadura, iVelocidad = 30, iHP = -30, precio = 10)
object ArcoViejo extends Item(ArmaDoble, iFuerza = 2, precio = 15)

object Maldito extends Item(Talisman, precio = 100) {
  override def HP(heroe: Heroe, valor: Double) = 1
  override def velocidad(heroe: Heroe, valor: Double) = 1
  override def inteligencia(hereo: Heroe, valor: Double) = 1
  override def fuerza(hereo: Heroe, valor: Double) = 1
}

object EscudoAntiRobo extends Item(ArmaSimple, iHP = 20, precio = 30) {
  override def cumpleCondicion(heroe: Heroe) = {
    val condicion = for(trabajo <- heroe.job
      if(trabajo == Ladron)
      if(heroe.fuerza < 20))
    yield heroe
    condicion.isEmpty
  }
}

object Dedicacion extends Item(Talisman, precio = 40) {
  
  def porcentaje(heroe: Heroe) = heroe.desequipar(this).statPrincipal.getOrElse(0.0) * 0.1
  override def HP(heroe: Heroe, valor: Double) = valor + porcentaje(heroe)
  override def fuerza(heroe: Heroe, valor: Double) = valor + porcentaje(heroe)
  override def velocidad(heroe: Heroe, valor: Double) = valor + porcentaje(heroe)
  override def inteligencia(heroe: Heroe, valor: Double) = valor + porcentaje(heroe)
}

object Minimalismo extends Item(Talisman, precio = 5) {
  override def HP(heroe: Heroe, valor: Double) = valor + 50 - 10 * (heroe.inventario.items.size - 1)
}

object VinchaDelBufaloDelAgua extends Item(Cabeza, precio = 50) {
  override def cumpleCondicion(heroe: Heroe) = heroe.job.isEmpty
  
  override def inteligencia(heroe: Heroe, valor: Double) = if(heroe.fuerza > heroe.inteligencia) valor + 30 else valor
  override def HP(heroe: Heroe, valor: Double) = if(heroe.fuerza <= heroe.inteligencia) valor + 10 else valor
  override def fuerza(heroe: Heroe, valor: Double) = if(heroe.fuerza <= heroe.inteligencia) valor + 10 else valor
  override def velocidad(heroe: Heroe, valor: Double) = if(heroe.fuerza <= heroe.inteligencia) valor + 10 else valor
}

object EspadaDeLaVida extends Item(ArmaSimple, precio = 20) {
  override def fuerza(heroe: Heroe, valor: Double) = heroe.HPFinal
}

