package tadQuest

trait Tarea {
  def facilidadPara(equipo: Equipo): Option[Heroe => Double]
  def afectar(heroe: Heroe): Heroe = heroe
}

case object PelearContraMonstruo extends Tarea {
  def facilidadPara(equipo: Equipo) = (
    for {lider <- equipo.lider; trabajo <- lider.job 
      if trabajo eq Guerrero} 
    yield (h => 20): Heroe => Double).orElse(Some(h => 10)
  )
  
  override def afectar(heroe: Heroe) = {
    if(heroe.statFinal(StatFuerza) < 20) heroe.modificarStats(IncrementoStats(-10, 0, 0, 0))
    else heroe
  }
}

case object ForzarPuerta extends Tarea {
  def facilidadPara(equipo: Equipo) = {
    Some(_.statFinal(StatInteligencia) + 10 * equipo.miembrosConTrabajo.count(_.job.get == Ladron))
  }
  override def afectar(heroe: Heroe) = (
    for{trabajo <- heroe.job
      if List(Mago, Ladron).contains(trabajo)} 
    yield heroe).getOrElse(heroe.modificarStats(IncrementoStats(-5, 1, 0, 0)))
}

case class RobarTalisman(talisman: Item) extends Tarea {
  def facilidadPara(equipo: Equipo) = for {
    lider <- equipo.lider; trabajo <- lider.job 
    if trabajo eq Ladron
  } yield (_:Heroe).statFinal(StatVelocidad)
  override def afectar(heroe: Heroe) = heroe.equipar(talisman)
}