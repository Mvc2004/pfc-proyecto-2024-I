package proyecto


import datos._

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    def rutas(origen: String, destino: String, visitados: Set[String], rutaActual: List[Vuelo]): List[vuelos] = {
      if (origen == destino) {
        List(rutaActual)
      } else {
        vuelos.filter(v => v.Org == origen && !visitados.contains(v.Dst)).flatMap { vuelo =>
          rutas(vuelo.Dst, destino, visitados + origen, rutaActual :+ vuelo)
        }
      }
    }

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      rutas(aeropuertoOrigen, aeropuertoDestino, Set(), List())
    }
  }

  def calcularDuracionVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Int = {
    val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
    val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

    val salidaMin = (vuelo.HS * 60) + vuelo.MS
    val llegadaMin = (vuelo.HL * 60) + vuelo.ML

    val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
    val diferenciaGMTMin = (diferenciaGMT * 60).toInt

    val duracionMin = llegadaMin - (salidaMin + diferenciaGMTMin)

    if (duracionMin < 0) duracionMin + 1440 else duracionMin
  }

  def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
    val llegadaMin = vuelo1.HL * 60 + vuelo1.ML
    val salidaMin = vuelo2.HS * 60 + vuelo2.MS

    val esperaMin = salidaMin - llegadaMin

    if (esperaMin < 0) esperaMin + 1440 else esperaMin
  }

  def calcularTiempoTotal(ruta: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    val tiemposVuelo = ruta.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos))
    val tiemposEspera = ruta.zip(ruta.tail).map { case (v1, v2) => calcularTiempoEspera(v1, v2) }
    tiemposVuelo.sum + tiemposEspera.sum
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      buscarItinerarios(aeropuertoOrigen, aeropuertoDestino).sortBy(ruta => calcularTiempoTotal(ruta, aeropuertos)).take(3)
    }
  }

  def calcularEscalas(ruta: List[Vuelo]): Int = {
    val escalas = ruta.map(_.Esc).sum
    val transiciones = ruta.size - 1
    escalas + transiciones
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      buscarItinerarios(aeropuertoOrigen, aeropuertoDestino).sortBy(calcularEscalas).take(3)
    }
  }

  def calcularTiempoVueloTotal(ruta: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    ruta.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos)).sum
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      buscarItinerarios(aeropuertoOrigen, aeropuertoDestino).sortBy(ruta => calcularTiempoVueloTotal(ruta, aeropuertos)).take(3)
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

      itinerariosValidos
        .sortBy { it =>
          val horaLlegada = calcularHoraLlegadaTotal(it)
          val lapsoTiempo = calcularLapsoTiempo(horaLlegada, tiempoCita)
          (lapsoTiempo, calcularHoraSalidaTotal(it))
        }
        .headOption
        .getOrElse(List.empty)
    }
  }
}
