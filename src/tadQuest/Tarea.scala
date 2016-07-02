package tadQuest

trait Tarea {
  def facilidadPara(equipo: Equipo): Option[Heroe => Double]
  def afectar(heroe: Heroe): Heroe = heroe
}

case object PelearContraMonstruo extends Tarea {
  def facilidadPara(equipo: Equipo): Option[Heroe => Double] = {
    val facilidad = for {lider <- equipo.lider; trabajo <- lider.job 
      if trabajo eq Guerrero
    } yield (h => 20): Heroe => Double
    facilidad.orElse(Some(h => 10))
  }
  override def afectar(heroe: Heroe): Heroe = {
    if(heroe.fuerzaFinal < 20) heroe.modificarStats(-10, 0, 0, 0)
    else heroe
  }
}

case object ForzarPuerta extends Tarea {
  def facilidadPara(equipo: Equipo): Option[Heroe => Double] = {
    val incremento = equipo.miembrosConTrabajo.count(_.job.get eq Ladron)
    Some(_.inteligenciaFinal + 10 * incremento)
  }
  override def afectar(heroe: Heroe): Heroe = {
    if(List(Mago, Ladron).contains(heroe.job.get)) heroe
    else heroe.modificarStats(-5, 1, 0, 0)
  }
}

case class RobarTalisman(talisman: Item) extends Tarea {
   def facilidadPara(equipo: Equipo): Option[Heroe => Double] = for {
     lider <- equipo.lider; trabajo <- lider.job 
     if trabajo eq Ladron
   } yield (_:Heroe).velocidadFinal
   override def afectar(heroe: Heroe): Heroe = heroe.equipar(talisman)
}
 
case object MatarAlDragon extends Tarea {
  def facilidadPara(equipo: Equipo): Option[Heroe => Double] = {
    if (equipo.heroes.size >= 1) Some(_.fuerzaFinal * 50)
    else Some(_.fuerzaFinal * 10)
  }
  override def afectar(heroe: Heroe): Heroe = heroe.modificarStats(1000, 1000, 1000, 1000)
}



