package tadQuest

object Main {
  
  
  def main(args: Array[String]): Unit = {
    
    val kratos = new Heroe(50, 45, 10, 10)
    val icaros = new Heroe(50000, 1, 10000, 10000)
    val spiderman = new Heroe(10, 35, 60, 40)
    val ironMan = new Heroe(50, 10, 40, 100)
    val capitanAmerica = new Heroe(70, 31, 60, 20)
    val wolverine = new Heroe(1000, 60, 50, 20)
    val otroEquipo = new Equipo("otro", List(spiderman.asignarTrabajo(Mago).equipar(PalitoMagico).equipar(EscudoAntiRobo),
      wolverine.asignarTrabajo(Ladron).equipar(EspadaDeLaVida), capitanAmerica.asignarTrabajo(Guerrero).equipar(CascoVikingo),
      kratos.equipar(VinchaDelBufaloDelAgua).equipar(ArcoViejo)))
    
    
    val heroe = new Heroe(10, 20, 30, 40)
    val otro = new Heroe(1,2,3,4)
    val equipo = new Equipo("nom", List(heroe))
 

    println(MatarAlDragon.facilidadPara(otroEquipo).get(heroe))
    
    
    
    
    
    
    
  }
  
  
  
  
  
}