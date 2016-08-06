package tadQuest

import org.junit.{Test, Before}
import scala.util.{Try, Failure}
import org.junit.Assert._

class UnTest {

  var spiderman: Heroe = _
  var ironMan: Heroe = _
  var capitanAmerica: Heroe = _
  var wolverine: Heroe = _
  var kratos: Heroe = _
  var equipo: Equipo = _
  var otroEquipo: Equipo = _
  var equipito: Equipo = _
  var equipo2: Equipo = _
  var icaros: Heroe = _
  var grupo: Equipo = _

  @Before
  def setup() = {
    grupo = Equipo("equipo", List(Heroe(1, 1, 1, 1), Heroe(2, 0, 0, 0)))
    kratos = Heroe(50, 45, 10, 10)
    icaros = Heroe(50000, 1, 10000, 10000)
    spiderman = Heroe(10, 35, 60, 40)
    ironMan = Heroe(50, 10, 40, 100)
    capitanAmerica = Heroe(70, 31, 60, 20)
    wolverine = Heroe(1000, 60, 50, 20)
    equipo = Equipo("vengadores_2", List(spiderman, ironMan))
    equipo2 = Equipo("Konami", List(icaros.asignarTrabajo(Guerrero), spiderman))
    equipito = Equipo("", List(spiderman.asignarTrabajo(Guerrero), ironMan.asignarTrabajo(Mago)))
    otroEquipo = Equipo("otro", List(spiderman.asignarTrabajo(Mago).equipar(PalitoMagico).equipar(EscudoAntiRobo),
      wolverine.asignarTrabajo(Ladron).equipar(EspadaDeLaVida), capitanAmerica.asignarTrabajo(Guerrero).equipar(CascoVikingo),
      kratos.equipar(VinchaDelBufaloDelAgua).equipar(ArcoViejo)))
  }

  @Test
  def testHeroeConsistenteFrenteACambios() {
    assertEquals(capitanAmerica.equipar(VinchaDelBufaloDelAgua).asignarTrabajo(Guerrero).inventario.items.size, 0, 0.01)
  }

  @Test
  def heroeSeConvierteEnGuerrero() {
    assertEquals(Some(Guerrero), capitanAmerica.asignarTrabajo(Guerrero).job)
  }

  @Test
  def heroeCambiaDeStatsCuandoSeConvierteEnMago() = {
    assertEquals(40, capitanAmerica.asignarTrabajo(Mago) statFinal StatInteligencia, 0.01)
  }

  @Test
  def heroeNoSeEquipaCascoVikingoPorNoCumplirCondicion() = {
    assertTrue(Try(ironMan.equipar(CascoVikingo)).
      transform(e => Failure(NoSePudoEquiparUnItem), f => Try(ironMan)).get.inventario.items.isEmpty)
  }

  @Test
  def heroeSeEquipaCascoVikingoYCumpleCondicion() = {
    assertTrue(spiderman.equipar(CascoVikingo).inventario.items.contains(CascoVikingo))
  }

  @Test
  def heroeSeEquipaCascoVikingoYCambiaSusStats() = {
    assertEquals(spiderman.equipar(CascoVikingo) statFinal StatHP, 20, 0.01)
  }

  @Test
  def heroeEquipaDosArmasSimples() = {
    assertEquals(wolverine.asignarTrabajo(Mago).equipar(PalitoMagico).equipar(EscudoAntiRobo).
      inventario.items.size, 2, 0.01)
  }

  @Test
  def heroeSeEquipaCascoVikingoYEscudoAntiRoboYCambiaSusStats() = {
    assertEquals(spiderman.asignarTrabajo(Guerrero).equipar(CascoVikingo).equipar(EscudoAntiRobo)
        statFinal StatHP, 50, 0.01)
  }

  @Test
  def heroeNoCumpleCondicion() = {
    assertEquals(Try(ironMan.asignarTrabajo(Guerrero).equipar(CascoVikingo)).
      transform(e => Failure(NoSePudoEquiparUnItem), f => Try(ironMan.asignarTrabajo(Guerrero))).get statFinal StatHP, 60, 0.01)
  }

  @Test
  def equiparEspadaDeLaVida() = {
    assertEquals(ironMan.equipar(EspadaDeLaVida) statFinal StatFuerza, 50, 0.01)
  }

  @Test
  def equiparVincha() = {
    assertEquals(ironMan.equipar(VinchaDelBufaloDelAgua) statFinal StatHP, 60, 0.01)
  }

  @Test
  def equiparTalismanMinimalismo() = {
    assertEquals(ironMan.asignarTrabajo(Guerrero).equipar(Minimalismo) statFinal StatHP, 110, 0.01)
  }

  @Test
  def equiparUnItemYUnTalisman() = {
    assertEquals(spiderman.asignarTrabajo(Guerrero).equipar(CascoVikingo).equipar(Minimalismo)
        statFinal StatHP, 70, 0.01)
  }

  @Test
  def heroeSeConvierteEnMagoYPuedeUsarCascoVikingo() = {
    assertEquals(spiderman.asignarTrabajo(Mago).equipar(CascoVikingo) statFinal StatHP, 20, 0.01)
  }

  @Test
  def heroeSeEquipaArmadura() = {
    assertEquals(ironMan.equipar(ArmaduraEleganteSport) statFinal StatVelocidad, 70, 0.01)
  }

  @Test
  def heroeSeEquipaPaloMagicoSiendoLadron() = {
    assertEquals(spiderman.asignarTrabajo(Ladron).equipar(PalitoMagico) statFinal StatInteligencia, 60, 0.01)
  }

  @Test
  def heroeConVariosItemsEsAfectadoPorElTalismanDelMinimalismo() = {
    assertEquals(capitanAmerica.equipar(EspadaDeLaVida).equipar(ArmaduraEleganteSport).
      equipar(Minimalismo) statFinal StatHP, 70, 0.01)
  }

  @Test
  def heroeSeEquipaEspadaDeLaVida() = {
    assertEquals(wolverine.asignarTrabajo(Guerrero).equipar(EspadaDeLaVida) statFinal StatFuerza, 1010, 0.01)
  }

  @Test
  def heroeSeEquipaTalismanDeDedicacion() = {
    assertEquals(capitanAmerica.asignarTrabajo(Guerrero).equipar(Dedicacion) statFinal StatHP, 84.6, 0.0001)
    assertEquals(capitanAmerica.asignarTrabajo(Guerrero).equipar(Dedicacion) statFinal StatFuerza, 50.6, 0.0001)
    assertEquals(capitanAmerica.asignarTrabajo(Guerrero).equipar(Dedicacion) statFinal StatVelocidad, 64.6, 0.0001)
    assertEquals(capitanAmerica.asignarTrabajo(Guerrero).equipar(Dedicacion) statFinal StatInteligencia, 14.6, 0.0001)
  }

  @Test
  def heroeNoSePudoEquiparEscudoAntiRoboNiPalitoMagico() = {
    assertTrue(Try(ironMan.equipar(EscudoAntiRobo).equipar(PalitoMagico)).isFailure)
  }

  @Test
  def cumpleConEquiparEspadaYEscudo() = {
    assertEquals(capitanAmerica.asignarTrabajo(Mago).equipar(EscudoAntiRobo).
      equipar(PalitoMagico).inventario.items.size, 2, 0.01)
  }

  @Test
  def equiparArmaDobleReemplazaArmaSimple() = {
    assertEquals(capitanAmerica.equipar(EscudoAntiRobo).equipar(ArcoViejo).inventario.items.size, 1, 0.001)
  }

  @Test
  def equiparTresArmasSimplesDiferentes() = {
    assertEquals(capitanAmerica.equipar(EscudoAntiRobo).equipar(EscudoAntiRobo).
      equipar(EspadaDeLaVida).inventario.items.size, 2, 0.001)
  }

  @Test
  def equiparDosArmasSimplesIgualesYUnaDiferenteQueReemplazaAUna() = {
    assertEquals(capitanAmerica.equipar(EscudoAntiRobo).equipar(EscudoAntiRobo).
      equipar(EspadaDeLaVida).inventario.items.size, 2, 0.001)
  }

  @Test
  def equiparDosArmasSimplesIgualesYUnaDiferenteQueReemplazaAUnaObtengoHP() = {
    assertEquals(capitanAmerica.equipar(EscudoAntiRobo).equipar(EscudoAntiRobo).
      equipar(EspadaDeLaVida) statFinal StatHP, 90, 0.001)
  }

  @Test
  def heroeCambiaDeTrabajo() = {
    assertEquals(capitanAmerica.asignarTrabajo(Mago).asignarTrabajo(Guerrero).job, Some(Guerrero))
  }

  @Test
  def mejorHeroeSegun() = {
    assertEquals(equipito.mejorHeroeSegun(_.HP).get, ironMan.asignarTrabajo(Mago))
  }

  @Test
  def obtenerMiembro() = {
    assertTrue(equipo.agregarMiembro(wolverine).heroes.contains(wolverine))
  }

  @Test
  def seReemplazaUnMiembroDelEquipoPorOtro() = {
    assertFalse(equipo.reemplazar(spiderman, capitanAmerica).heroes.contains(spiderman))
    assertTrue(equipo.reemplazar(spiderman, capitanAmerica).heroes.contains(capitanAmerica))
  }

  @Test
  def equipoNoTieneLiderDefinido() = {
    assertEquals(equipo.lider, None)
  }

  @Test
  def equipoTieneUnLider() = {
    assertEquals(equipo.agregarMiembro(capitanAmerica.asignarTrabajo(Guerrero)).
      agregarMiembro(wolverine.asignarTrabajo(Mago)).lider.get, capitanAmerica.asignarTrabajo(Guerrero))
  }

  @Test
  def incrementarElPozoComunDelEquipo() = {
    assertEquals(equipo.incrementarPozo(CascoVikingo.precio).pozoComun, 5, 0.01)
  }

  @Test
  def obtenerItemYVender() = {
    val unEquipo = Equipo("equipo", List(Heroe(0, 0, 0, 0).asignarTrabajo(Guerrero),
      Heroe(0, 0, 0, 0).asignarTrabajo(Mago)))
    assertEquals(unEquipo.obtenerItem(ArcoViejo).heroes.filter(_.statFinal(StatHP) == 10).head statFinal StatFuerza, 17, 0.01)
  }

  @Test
  def obtenerItemYAsignarloAlMejorHeroe() = {
    val unEquipo = Equipo("equipo", List(Heroe(0, 0, 0, 0).asignarTrabajo(Guerrero).equipar(Maldito)))
    assertEquals(unEquipo.obtenerItem(ArcoViejo).pozoComun, 15, 0.01)
  }

  @Test
  def heroeRealizaTareaNoAfectaSusStats() = {
    assertEquals(wolverine.realizarTarea(PelearContraMonstruo) statFinal StatHP, 1000, 0.01)
  }

  @Test
  def heroeRealizaTareaYAfectaSusStats() = {
    assertEquals(ironMan.asignarTrabajo(Ladron).realizarTarea(PelearContraMonstruo) statFinal StatHP, 35, 0.01)
  }

  @Test
  def heroeFuerzaPuertaSeModificanSusStats() = {
    assertEquals(ironMan.asignarTrabajo(Guerrero).realizarTarea(ForzarPuerta) statFinal StatHP, 55, 0.01)
    assertEquals(ironMan.asignarTrabajo(Guerrero).realizarTarea(ForzarPuerta) statFinal StatFuerza, 26, 0.01)
  }

  @Test
  def heroeTieneFacilidadDePelearContraMonstruo() = {
    val unEquipo = Equipo("equipo", List(spiderman, wolverine))
    assertEquals(PelearContraMonstruo.facilidadPara(unEquipo).get(spiderman), 10, 0.01)
  }

  @Test
  def heroeTieneFacilidadDePelearContraMonstruoYEsLiderGuerrero() = {
    val unEquipo = Equipo("equipo", List(spiderman, wolverine.asignarTrabajo(Guerrero)))
    assertEquals(PelearContraMonstruo.facilidadPara(unEquipo).get(wolverine.asignarTrabajo(Guerrero)), 20, 0.01)
  }

  @Test
  def heroeQueNoEsElLiderPeleaContraMonstruo() = {
    val unEquipo = Equipo("equipo", List(spiderman.asignarTrabajo(Ladron), wolverine.asignarTrabajo(Mago)))
    assertEquals(PelearContraMonstruo.facilidadPara(unEquipo).get(wolverine.asignarTrabajo(Mago)), 10, 0.01)
  }

  @Test
  def heroeFuerzaPuertaEquipoHayUnLadron() = {
    val unEquipo = Equipo("equipo", List(spiderman.asignarTrabajo(Ladron), wolverine.asignarTrabajo(Mago)))
    assertEquals(ForzarPuerta.facilidadPara(unEquipo).get(spiderman.asignarTrabajo(Ladron)), 50, 0.01)
  }

  @Test
  def heroeFuerzaPuertaEquipoSinLadron() = {
    val unEquipo = Equipo("equipo", List(spiderman.asignarTrabajo(Guerrero), wolverine.asignarTrabajo(Mago)))
    assertEquals(ForzarPuerta.facilidadPara(unEquipo).get(spiderman.asignarTrabajo(Guerrero)), 30, 0.01)
  }

  @Test
  def facilidadDeHeroeQueRobaTalismanSiEquipoSiTieneLiderDefinido() = {
    val unEquipo = Equipo("equipo", List(spiderman.asignarTrabajo(Ladron), wolverine.asignarTrabajo(Mago)))
    assertEquals(RobarTalisman(Maldito).facilidadPara(unEquipo).get(spiderman.asignarTrabajo(Ladron)), 70, 0.01)
  }

  @Test
  def facilidadDeHereoQueRobaTalismanSiEquipoTieneLiderQueNoEsLadron() = {
    val unEquipo = Equipo("equipo", List(spiderman.asignarTrabajo(Mago), wolverine.asignarTrabajo(Mago)))
    assertEquals(RobarTalisman(Maldito).facilidadPara(unEquipo), None)
  }

  @Test
  def facilidadDeHeroeQueRobaTalismanSiEquipoNoTieneLiderDefinido() = {
    val unEquipo = Equipo("equipo", List(spiderman, wolverine))
    assertEquals(RobarTalisman(Maldito).facilidadPara(unEquipo), None)
  }

  @Test
  def lider() = {
    assertEquals(otroEquipo.lider.get, spiderman.asignarTrabajo(Mago).equipar(PalitoMagico).equipar(EscudoAntiRobo))
  }

  @Test
  def equipoNoPuedeRealizarTareaSuLiderNoEsLadri() = {
    assertEquals(otroEquipo.elMejorPuedeRealizar(RobarTalisman(Maldito)), None)
  }

  @Test
  def equipoNoPuedePelearContraMonstruoNoHayCandidatoMejorSegun() = {
    assertEquals(otroEquipo.elMejorPuedeRealizar(PelearContraMonstruo),
      Some(spiderman.asignarTrabajo(Mago).equipar(PalitoMagico).equipar(EscudoAntiRobo)))
  }

  @Test
  def equipoPuedeRealizarLaTareaDeForzarPuerta() = {
    assertTrue(otroEquipo.elMejorPuedeRealizar(ForzarPuerta).isDefined)
  }

  @Test
  def equipoEsModificadoSiRealizaUnaMision() = {
    assertEquals(equipo2.realizarMision(new Mision(List(PelearContraMonstruo), GanarOroParaElPozoComun(100)))
      .get.pozoComun, 100, 0.01)
  }

  @Test
  def equipoNoPuedeRealizarUnaMisionSinLider() = {
    assertTrue(equipo.realizarMision(new Mision(List(PelearContraMonstruo, ForzarPuerta, RobarTalisman(Dedicacion),
      PelearContraMonstruo), GanarOroParaElPozoComun(1000))).isFailure)
  }

  @Test
  def equipoNoPuedeRealizarUnaTareaYLaInforma() = {
    assertEquals(grupo.realizarMision(new Mision(List(RobarTalisman(Maldito)), GanarOroParaElPozoComun(10))) match {
      case NoPudoRealizar(_, tareaFallida) => tareaFallida
      case _ =>
    }, RobarTalisman(Maldito))
  }

  @Test
  def testElegirMision() = {
    val mision1 = new Mision(List(PelearContraMonstruo), GanarOroParaElPozoComun(100))
    val mision2 = new Mision(List(PelearContraMonstruo), GanarOroParaElPozoComun(1000))
    val taberna = Taberna(List(mision1, mision2))
    assertEquals(taberna.elegirMision((e1, e2) => grupo.pozoComun > grupo.pozoComun, grupo).get, mision2)
  }

  @Test
  def noPuedeRealizarMision() = {
    val mision = new Mision(List(RobarTalisman(Maldito)), GanarOroParaElPozoComun(10))
    val taberna = Taberna(List(mision))
    assertEquals(taberna.elegirMision((e1, e2) => grupo.pozoComun > grupo.pozoComun, grupo), None)
  }

  @Test
  def equipoPuedeEntrenar() = {
    val mision1 = new Mision(List(PelearContraMonstruo), GanarOroParaElPozoComun(100))
    val taberna = Taberna(List(mision1))
    assertEquals(grupo.entrenar(taberna, (e1, e2) => grupo.pozoComun > grupo.pozoComun),
      grupo.realizarMision(mision1).get)
  }

  @Test
  def tabernaSinMisionesEquipoNoCambiaDeEstado() = {
    assertEquals(grupo.entrenar(Taberna(Nil), (_, _) => true), grupo)
  }

  @Test
  def equipoRealizaVariasMisionesAumentandoSuPozoComun() = {
    val mision1 = new Mision(List(PelearContraMonstruo, PelearContraMonstruo), GanarOroParaElPozoComun(100))
    val mision2 = new Mision(List(PelearContraMonstruo), GanarOroParaElPozoComun(50))
    val taberna = Taberna(List(mision1, mision2))
    assertEquals(grupo.entrenar(taberna, (_, _) => grupo.pozoComun > grupo.pozoComun).pozoComun, 150, 0.01)
  }

}