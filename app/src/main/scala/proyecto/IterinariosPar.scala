package proyecto

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    def encontrarRutas(origen: String, destino: String, visitados: Set[String], rutaActual: List[Vuelo]): List[vuelos] = {
      if (origen == destino) {
        List(rutaActual)
      } else {
        vuelos.filter(v => v.Org == origen && !visitados.contains(v.Dst)).flatMap { vuelo =>
          encontrarRutas(vuelo.Dst, destino, visitados + origen, rutaActual :+ vuelo)
        }
      }
    }

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      encontrarRutas(aeropuertoOrigen, aeropuertoDestino, Set(), List())
    }
  }

  def calcularDuracionVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Int = {
    val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
    val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

    val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
    val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

    val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
    val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

    val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

    if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
  }

  def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
    val llegadaEnMinutos = vuelo1.HL * 60 + vuelo1.ML
    val salidaEnMinutos = vuelo2.HS * 60 + vuelo2.MS

    val esperaEnMinutos = salidaEnMinutos - llegadaEnMinutos

    if (esperaEnMinutos < 0) esperaEnMinutos + 1440 else esperaEnMinutos
  }

  def calcularTiempoTotal(ruta: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    val tiemposDeVueloFutures = ruta.map(vuelo => Future { calcularDuracionVuelo(vuelo, aeropuertos) })
    val tiemposDeEsperaFutures = ruta.zip(ruta.tail).map { case (v1, v2) => Future { calcularTiempoEspera(v1, v2) } }

    val tiemposDeVuelo = Await.result(Future.sequence(tiemposDeVueloFutures), 10.seconds)
    val tiemposDeEspera = Await.result(Future.sequence(tiemposDeEsperaFutures), 10.seconds)

    tiemposDeVuelo.sum + tiemposDeEspera.sum
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      val rutas = buscarItinerarios(aeropuertoOrigen, aeropuertoDestino)
      val rutasConTiempo = rutas.map(ruta => Future { (ruta, calcularTiempoTotal(ruta, aeropuertos)) })
      val rutasOrdenadas = Await.result(Future.sequence(rutasConTiempo), 10.seconds).sortBy(_._2).map(_._1)
      rutasOrdenadas.take(3)
    }
  }

  def calcularEscalas(ruta: List[Vuelo]): Int = {
    val escalasIndividuales = ruta.map(_.Esc).sum
    val transiciones = ruta.size - 1
    escalasIndividuales + transiciones
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      val rutas = buscarItinerarios(aeropuertoOrigen, aeropuertoDestino)
      val rutasConEscalas = rutas.map(ruta => Future { (ruta, calcularEscalas(ruta)) })
      val rutasOrdenadas = Await.result(Future.sequence(rutasConEscalas), 10.seconds).sortBy(_._2).map(_._1)
      rutasOrdenadas.take(3)
    }
  }

  def calcularTiempoTotalDeVuelo(ruta: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    ruta.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos)).sum
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      val rutas = buscarItinerarios(aeropuertoOrigen, aeropuertoDestino)
      val rutasConTiempoVuelo = rutas.map(ruta => Future { (ruta, calcularTiempoTotalDeVuelo(ruta, aeropuertos)) })
      val rutasOrdenadas = Await.result(Future.sequence(rutasConTiempoVuelo), 10.seconds).sortBy(_._2).map(_._1)
      rutasOrdenadas.take(3)
    }
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    val buscarItinerariosFn = itinerarios(vuelos, aeropuertos)

    val convertirAMinutos: (Int, Int) => Int = (hora, minutos) => hora * 60 + minutos

    val calcularHoraLlegadaTotal: List[Vuelo] => Int =
      itinerario => convertirAMinutos(itinerario.last.HL, itinerario.last.ML)

    val calcularHoraSalidaTotal: List[Vuelo] => Int =
      itinerario => convertirAMinutos(itinerario.head.HS, itinerario.head.MS)

    val calcularLapsoTiempo: (Int, Int) => Int = (horaLlegada, horaCita) => {
      val diferencia = horaCita - horaLlegada
      if (diferencia >= 0) diferencia else 1440 + diferencia
    }

    val esValido: (List[Vuelo], Int) => Boolean = (itinerario, tiempoCita) => {
      val horaLlegada = calcularHoraLlegadaTotal(itinerario)
      horaLlegada <= tiempoCita || (horaLlegada < 1440 && tiempoCita < horaLlegada)
    }

    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val todosItinerarios = buscarItinerariosFn(origen, destino)
      val itinerariosValidos = todosItinerarios.filter(it => esValido(it, tiempoCita))

      val rutasConLapsoTiempo = itinerariosValidos.map { it =>
        Future {
          val horaLlegada = calcularHoraLlegadaTotal(it)
          val lapsoTiempo = calcularLapsoTiempo(horaLlegada, tiempoCita)
          (it, lapsoTiempo, calcularHoraSalidaTotal(it))
        }
      }

      Await.result(Future.sequence(rutasConLapsoTiempo), 10.seconds)
        .sortBy { case (_, lapsoTiempo, horaSalida) => (lapsoTiempo, horaSalida) }
        .headOption
        .map(_._1)
        .getOrElse(List.empty)
    }
  }
}
