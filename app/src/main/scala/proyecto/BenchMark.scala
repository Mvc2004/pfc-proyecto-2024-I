package proyecto
import datos._
import org.scalameter._

import org.scalameter.{Key, Warmer, withWarmer}


class BenchMark {

  val objSecuencial = new Itinerario()
  val objParalelo = new ItinerariosPar()

  def itinerariosBenchMark(): Unit = {
    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerarios(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objParalelo.itinerariosPar(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba de Itinerarios con Lista de Vuelos C1 y Lista Aeropuertos, Buscando El Vuelo de PHX a LAX")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)
  }

  def itinerariosTiempoBenchMark(): Unit = {

    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objParalelo.itinerariosTiempoPar(vuelosD1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba Aeropuertos Curso Buscando El Vuelo ")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }

  def itinerariosEscalasBenchMark(): Unit = {
    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerariosEscalas(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objParalelo.itinerariosEscalasPar(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba Aeropuertos Curso Buscando El Vuelo ")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }

  def itinerariosAireBenchMark(): Unit = {

    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerariosAire(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objParalelo.itinerariosAirePar(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba Aeropuertos Curso Buscando El Vuelo ")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }

  def itinerariosSalidaBenchMark(): Unit = {

    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("DFW", "ATL", 15, 10)
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("ATL", "DFW", 15, 10)
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("DFW", "ATL", 15, 10)
      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("ATL", "DFW", 15, 10)
    }

    println("Prueba Aeropuertos Curso Buscando El Vuelo ")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }
  def itinerariosSalidaBenchmark(): Unit = {

    val timeParalela5 = config(
      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {


      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("BOS","SFO", 12, 22)
    }
    val timeSecuencial5 = config(

      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerariosSalida(vuelosCurso , aeropuertosCurso)
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("BOS","SFO", 12, 22)
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("BOS","SFO", 12, 22)
    }
    println(s"Tiempo de ejecución Salida P5: $timeParalela5")
    println(s"Tiempo de ejecución Salida S5: $timeSecuencial5")
  }

}