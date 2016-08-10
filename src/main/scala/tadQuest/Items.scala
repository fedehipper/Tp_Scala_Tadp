package tadQuest

object Sector extends Enumeration {
  type Sector = Value
  val Cabeza, Armadura, ArmaSimple, ArmaDoble, Talisman = Value
}
import Sector._

abstract class Item(val sector: Sector, val iFuerza: Double = 0, val iVelocidad: Double = 0,
    val iInteligencia: Double = 0, val iHP: Double = 0, val precio: Double = 0)
    extends MatcherStats(iHP, iVelocidad, iFuerza, iInteligencia){
  
  def statItem(statItem: Stat, heroe: Heroe, valor: Double) = matchStat(statItem) + valor
  def cumpleCondicion(heroe: Heroe): Boolean = true
}

object CascoVikingo extends Item(Cabeza, iHP = 10, precio = 5) {
  override def cumpleCondicion(heroe: Heroe) = heroe.fuerza > 30
}

object PalitoMagico extends Item(ArmaSimple, iInteligencia = 20, precio = 25) {
  override def cumpleCondicion(heroe: Heroe) = (
    for{trabajo <- heroe.job
      if trabajo == Mago || (trabajo == Ladron && heroe.inteligencia > 30)}
    yield heroe).isDefined
}

object ArmaduraEleganteSport extends Item(Armadura, iVelocidad = 30, iHP = -30, precio = 10)
object ArcoViejo extends Item(ArmaDoble, iFuerza = 2, precio = 15)

object Maldito extends Item(Talisman, precio = 100) {
  override def statItem(statItem: Stat, heroe: Heroe, valor: Double) = 1
}

object EscudoAntiRobo extends Item(ArmaSimple, iHP = 20, precio = 30) {
  override def cumpleCondicion(heroe: Heroe) = (
    for(trabajo <- heroe.job
      if trabajo == Ladron
      if heroe.fuerza < 20)
    yield heroe).isEmpty
}

object Dedicacion extends Item(Talisman, precio = 40) {
  def porcentaje(heroe: Heroe) = heroe.desequipar(this).statPrincipal.getOrElse(0.0) * 0.1
  override def statItem(statItem: Stat, heroe: Heroe, valor: Double) = valor + porcentaje(heroe)  
}

object Minimalismo extends Item(Talisman, precio = 5) {
  override def statItem(statItem: Stat, heroe: Heroe, valor: Double) = statItem match {
    case StatHP => 50 - 10 * (heroe.inventario.items.size - 1) + valor
    case _ => super.statItem(statItem, heroe, valor)
  }
}

object VinchaDelBufaloDelAgua extends Item(Cabeza, precio = 50) {
  override def cumpleCondicion(heroe: Heroe) = heroe.job.isEmpty
  
  override def statItem(statItem: Stat, heroe: Heroe, valor: Double) = statItem match {
    case StatInteligencia => if(heroe.fuerza > heroe.inteligencia) valor + 30 else valor
    case _ => if(heroe.fuerza <= heroe.inteligencia) valor + 10 else valor
  }
}

object EspadaDeLaVida extends Item(ArmaSimple, precio = 20) {
  override def statItem(statItem: Stat, heroe: Heroe, valor: Double) = statItem match {
    case StatFuerza => heroe.statFinal(StatHP)
    case _ => super.statItem(statItem, heroe, valor)
  }
}