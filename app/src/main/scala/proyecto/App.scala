
/**
 * Taller 3 - Programación Funcional
 * Autores: <Estudiantes>
 * Profesor: Carlos A Delgado
 */
package proyecto
import datos._

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object App {

  def saludo() = "Proyecto final"

  def main(args: Array[String]): Unit = {
    println(saludo())
    println(vuelosCurso)
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )
    val itinerarioSeq = new Itinerario()
    val itinerarioPar = new ItinerariosPar()
    val pruebas = new BenchMark

    val (seq1, par1) = pruebas.medirItinerarios(itinerarioSeq.itinerarios(vuelosC1, aeropuertos), itinerarioPar.itinerariosPar(vuelosC1, aeropuertos))
    val (seq2, par2) = pruebas.medirItinerarios(itinerarioSeq.itinerariosTiempo(vuelosC1, aeropuertos), itinerarioPar.itinerariosTiempoPar(vuelosC1, aeropuertos))
    val (seq3, par3) = pruebas.medirItinerarios(itinerarioSeq.itinerariosEscalas(vuelosC1, aeropuertos), itinerarioPar.itinerariosEscalasPar(vuelosC1, aeropuertos))
    val (seq4, par4) = pruebas.medirItinerarios(itinerarioSeq.itinerariosAire(vuelosC1, aeropuertos), itinerarioPar.itinerariosAirePar(vuelosC1, aeropuertos))
    val (seq5, par5) = pruebas.medirItinerariosSalida(itinerarioSeq.itinerariosSalida(vuelosC1, aeropuertos), itinerarioPar.itinerariosSalidaPar(vuelosC1, aeropuertos))
    println(pruebas.mostrarResultado(seq1, par1, "itinerarios"))
    println(pruebas.mostrarResultado(seq2, par2, "itinerariosTiempo"))
    println(pruebas.mostrarResultado(seq3, par3, "itinerariosEscalas"))
    println(pruebas.mostrarResultado(seq4, par4, "itinerariosAire"))
    println(pruebas.mostrarResultado(seq5, par5, "itinerariosSalida"))
  }
}