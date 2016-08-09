package tadQuest

case class Equipo(nombre: String, heroes: List[Heroe] = Nil, pozoComun: Double = 0) {
  
  def agregarMiembro(unMiembro: Heroe) = copy(heroes = unMiembro :: heroes) 
  def miembrosConTrabajo = heroes.filter(_.job.isDefined)
  def reemplazar(viejo: Heroe, nuevo: Heroe) = copy(heroes = nuevo :: heroes.filterNot(_ == viejo))
  def maximo = heroes.map(_: Heroe => Double).max
  def mejorHeroeSegun(cuantificador: Heroe => Double) = heroes.find(cuantificador(_) == maximo(cuantificador))
  def equiparATodos(item: Item) = copy(heroes = heroes.map(_.equipar(item)))
  def incrementarPozo(cantidad: Double) = copy(pozoComun = pozoComun + cantidad)
  def incrementoStat(heroe: Heroe, item: Item) = heroe.equipar(item).statPrincipal.get - heroe.statPrincipal.get
  def cobrarRecompensa(mision: Mision): Equipo = mision.recompensa.cobrar(this)
  
  def lider: Option[Heroe] = {
    if (miembrosConTrabajo.nonEmpty) 
      copy(heroes = miembrosConTrabajo).mejorHeroeSegun(_.statPrincipal.get)
    else None
  }
<<<<<<< HEAD

  def mejorHeroeSegun(quantifier: Heroe => Double) = heroes.find(quantifier(_) == heroes.map(quantifier(_)).max)
  
  def incrementarPozo(cantidad: Double) = copy(pozoComun = pozoComun + cantidad)
=======
>>>>>>> 384aad53d1c094b85f2a30b943bbacb267be0aad
  
  def incrementarStatsMiembros(condicion: Heroe => Boolean, recompensa: IncrementoStats) = {
    copy(heroes = for(heroe <- heroes if condicion(heroe)) 
      yield heroe modificarStats recompensa)
  }
  
  def obtenerItem(item: Item): Equipo = (
    for(heroe <- mejorHeroeSegun(incrementoStat(_, item)) 
      if incrementoStat(heroe, item) > 0)
    yield reemplazar(heroe, heroe equipar item)).getOrElse(incrementarPozo(item.precio))
  
  def elMejorPuedeRealizar(tarea: Tarea): Option[Heroe] = {
    for(facilidad <- tarea facilidadPara this; mejor <- mejorHeroeSegun(facilidad))
    yield mejor
  }
  
<<<<<<< HEAD
  val cobrarRecompensa = (_:Mision).recompensa cobrar this
  
  def realizarMision(mision: Mision): ResultadoMision = 
=======
  def realizarMision(mision: Mision): ResultadoMision = { 
>>>>>>> 384aad53d1c094b85f2a30b943bbacb267be0aad
    mision.tareas.foldLeft(CumpleMision(this): ResultadoMision)((anterior, tarea) => anterior match {
      case FallaMision(_,_) => anterior
      case _ => postTarea(anterior.get, tarea).fold(anterior.falloTarea(tarea))(team => anterior.map(_ => team))
    }
  ).terminar(mision)}

  def postTarea(equipo: Equipo, tarea: Tarea) = {
    for(heroe <- equipo elMejorPuedeRealizar tarea) 
    yield equipo.reemplazar(heroe, heroe realizarTarea tarea)
  }
  
  def entrenar(taberna: Taberna, criterio: (Equipo, Equipo) => Boolean): Equipo = (
<<<<<<< HEAD
    for{misionElegida <- taberna.elegirMision(criterio, this)
       equipo <- realizarMision(misionElegida).toOption}
    yield equipo.entrenar(taberna misionRealizada misionElegida, criterio)).getOrElse(this)

=======
    for(misionElegida <- taberna.elegirMision(criterio, this); equipo <- realizarMision(misionElegida).toOption)
      yield equipo.entrenar(taberna.misionRealizada(misionElegida), criterio)).getOrElse(this)
  
>>>>>>> 384aad53d1c094b85f2a30b943bbacb267be0aad
}