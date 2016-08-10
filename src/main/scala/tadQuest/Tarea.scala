package tadQuest

trait Tarea {
  def facilidadPara(equipo: Equipo): Option[Heroe => Double]
  def afectar(heroe: Heroe): Heroe
}

case object PelearContraMonstruo extends Tarea {
  def facilidadPara(equipo: Equipo) = (
    for {lider <- equipo.lider; trabajo <- lider.job 
      if trabajo eq Guerrero} 
    yield (_ => 20): Heroe => Double).orElse(Some(_ => 10))
  
  def afectar(heroe: Heroe) = {
    if(heroe.statFinal(StatFuerza) < 20) heroe.modificarStats(IncrementoStats(-10, 0, 0, 0))
    else heroe
  }
}

case object ForzarPuerta extends Tarea {
  def facilidadPara(equipo: Equipo) = {
    val incremento = equipo.heroesTrabajando.count(_.job.get == Ladron)
    Some(_.statFinal(StatInteligencia) + 10 * incremento)
  }
  def afectar(heroe: Heroe) = (
    for{trabajo <- heroe.job
      if List(Mago, Ladron).contains(trabajo)} 
    yield heroe).getOrElse(heroe.modificarStats(IncrementoStats(-5, 1, 0, 0)))
}

case class RobarTalisman(talisman: Item) extends Tarea {
  def facilidadPara(equipo: Equipo) = for {
    lider <- equipo.lider; trabajo <- lider.job 
    if trabajo eq Ladron
  } yield _.statFinal(StatVelocidad)
  def afectar(heroe: Heroe) = heroe.equipar(talisman)
}
 
case object MatarAlDragon extends Tarea {
  def facilidadPara(equipo: Equipo) = {
    if (equipo.heroes.size > 5) Some(_.statFinal(StatFuerza) * 50)
    else Some(_.statFinal(StatFuerza) * 10)
  }
  def afectar(heroe: Heroe) = heroe.modificarStats(IncrementoStats(1000, 1000, 1000, 1000))
}