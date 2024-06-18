package proyecto

class Itinerario {

  type Aeropuertos = List[Aeropuerto]
  type Vuelos = List[Vuelo]

  def encontrarZonaHoraria(codigo: String, aeropuertos: Aeropuertos): Int = {
    aeropuertos.find(_.codigo == codigo).map(_.zonaHoraria).getOrElse(0)
  }

  def convertirAHoraLocal(hora: Int, minuto: Int, zonaOrigen: Int, zonaDestino: Int): (Int, Int) = {
    val totalMinutosOrigen = hora * 60 + minuto + zonaOrigen * 60
    val totalMinutosDestino = totalMinutosOrigen + (zonaDestino - zonaOrigen) * 60
    val horaDestino = (totalMinutosDestino / 60) % 24
    val minutoDestino = totalMinutosDestino % 60
    (horaDestino, minutoDestino)
  }

  def addMinutes(hour: Int, minute: Int, minutesToAdd: Int): (Int, Int) = {
    val totalMinutes = hour * 60 + minute + minutesToAdd
    val newHour = (totalMinutes / 60) % 24
    val newMinute = totalMinutes % 60
    (newHour, newMinute)
  }

  def isFlightValid(flight: Vuelo, currentCity: String, currentTime: (Int, Int)): Boolean = {
    val (currentHour, currentMinute) = currentTime
    flight.origen == currentCity && (
      flight.horaSalida > currentHour || (flight.horaSalida == currentHour && flight.minutoSalida >= currentMinute)
      )
  }

  def findRoutesRec(
                     flights: Vuelos,
                     currentCity: String,
                     currentTime: (Int, Int),
                     visited: Set[Vuelo],
                     arrival: String
                   ): List[List[Vuelo]] = {
    if (currentCity == arrival) {
      List(List.empty[Vuelo])
    } else {
      flights
        .filter(f => isFlightValid(f, currentCity, currentTime) && !visited.contains(f))
        .flatMap { flight =>
          val arrivalTime = addMinutes(flight.horaLlegada, flight.minutoLlegada, flight.dias * 24 * 60)
          val routes = findRoutesRec(flights, flight.destino, arrivalTime, visited + flight, arrival)
          routes.map(flight :: _)
        }
    }
  }


  def itinerarios(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[List[Vuelo]] = {
    (cod1: String, cod2: String) => {
      findRoutesRec(vuelos, cod1, (0, 0), Set.empty, cod2).filter(_.nonEmpty)
    }
  }

  def itinerariosTiempo(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[List[Vuelo]] = {
    (cod1: String, cod2: String) => {
      val rutas = findRoutesRec(vuelos, cod1, (0, 0), Set.empty, cod2)
      val rutasConTiempo = rutas.map { ruta =>
        val (tiempoTotal, _) = ruta.foldLeft((0, (0, 0))) { case ((tiempoAcumulado, horaMinutoPrevio), vuelo) =>
          val zonaOrigen = encontrarZonaHoraria(vuelo.origen, aeropuertos)
          val zonaDestino = encontrarZonaHoraria(vuelo.destino, aeropuertos)

          // Convertir horas de salida y llegada a la hora local
          val (horaSalidaLocal, minutoSalidaLocal) = convertirAHoraLocal(vuelo.horaSalida, vuelo.minutoSalida, zonaOrigen, zonaDestino)
          val (horaLlegadaLocal, minutoLlegadaLocal) = convertirAHoraLocal(vuelo.horaLlegada, vuelo.minutoLlegada, zonaOrigen, zonaDestino)

          // Calcular el tiempo de vuelo para este segmento
          val tiempoVuelo = ((horaLlegadaLocal - horaSalidaLocal) * 60) + (minutoLlegadaLocal - minutoSalidaLocal)

          // Calcular el tiempo de espera desde el vuelo anterior, si existe
          val (horaPrevio, minutoPrevio) = horaMinutoPrevio
          val tiempoEspera = ((horaSalidaLocal - horaPrevio) * 60) + (minutoSalidaLocal - minutoPrevio)

          (tiempoAcumulado + tiempoVuelo + tiempoEspera, (horaLlegadaLocal, minutoLlegadaLocal))
        }
        (ruta, tiempoTotal)
      }
      // Ordenar las rutas por tiempo total y devolver solo las primeras tres rutas
      rutasConTiempo.sortBy(_._2).take(3).map(_._1)
    }
  }


  def itinerariosEscalas(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[List[Vuelo]] = {
    (cod1: String, cod2: String) => {
      val rutas = findRoutesRec(vuelos, cod1, (0, 0), Set.empty, cod2)
      val rutasConEscalas = rutas.map(ruta => (ruta, ruta.size - 1))
      rutasConEscalas.sortBy(_._2).take(3).map(_._1)
    }
  }

  def itinerariosAire(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[List[Vuelo]] = {
    (cod1: String, cod2: String) => {
      val rutas = findRoutesRec(vuelos, cod1, (0, 0), Set.empty, cod2)
      val rutasConTiempoAire = rutas.map { ruta =>
        val tiempoAire = ruta.map { vuelo =>
          val (horaSalidaLocal, minutoSalidaLocal) = (vuelo.horaSalida, vuelo.minutoSalida)
          val (horaLlegadaLocal, minutoLlegadaLocal) = (vuelo.horaLlegada, vuelo.minutoLlegada)
          val tiempoVuelo = ((horaLlegadaLocal - horaSalidaLocal) * 60) + (minutoLlegadaLocal - minutoSalidaLocal)
          tiempoVuelo
        }.sum
        (ruta, tiempoAire)
      }
      rutasConTiempoAire.sortBy(_._2).take(3).map(_._1)
    }
  }

  def itinerariosSalida(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String, Int, Int) => List[List[Vuelo]] = {
    (cod1: String, cod2: String, HC: Int, MC: Int) => {
      val rutas = findRoutesRec(vuelos, cod1, (0, 0), Set.empty, cod2)
      val rutasConLlegadaATiempo = rutas.filter { ruta =>
        val vueloFinal = ruta.last
        val (horaLlegada, minutoLlegada) = (vueloFinal.horaLlegada, vueloFinal.minutoLlegada)
        horaLlegada < HC || (horaLlegada == HC && minutoLlegada <= MC)
      }
      rutasConLlegadaATiempo.take(3)
    }
  }

  def itinerarioMasTemprano(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[Vuelo] = {
    (origen: String, destino: String) =>
      vuelos.filter(_.origen == origen)
        .filter(_.destino == destino)
        .sortBy(v => (v.horaSalida, v.minutoSalida))
        .take(1)
  }
}
