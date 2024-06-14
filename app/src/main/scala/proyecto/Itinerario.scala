package proyecto

import datos._

class Itinerario() {

  type Aeropuertos = List[Aeropuerto]
  type Vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
      if (origen == destino) return List(List())
      if (visitados.contains(origen)) return List()

      val vuelosDesdeOrigen = vuelos.filter(_.Org == origen)
      vuelosDesdeOrigen.flatMap { vuelo =>
        val subitinerarios = buscarItinerarios(vuelo.Dst, destino, visitados + origen)
        subitinerarios.map(subitinerario => vuelo :: subitinerario)
      }
    }

    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2)
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    val mapaAeropuertos = aeropuertos.map(a => a.Cod -> a).toMap

    def calcularTiempoVuelo(vuelo: Vuelo): Int = {
      val origen = mapaAeropuertos(vuelo.Org)
      val destino = mapaAeropuertos(vuelo.Dst)
      val horaSalidaGMT = vuelo.HS * 60 + vuelo.MS + (origen.GMT * 60).toInt
      val horaLlegadaGMT = vuelo.HL * 60 + vuelo.ML + (destino.GMT * 60).toInt
      horaLlegadaGMT - horaSalidaGMT
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario.map(calcularTiempoVuelo).sum
    }

    def buscarItinerarios(origen: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
      if (origen == destino) return List(List())
      if (visitados.contains(origen)) return List()

      val vuelosDesdeOrigen = vuelos.filter(_.Org == origen)
      vuelosDesdeOrigen.flatMap { vuelo =>
        val subitinerarios = buscarItinerarios(vuelo.Dst, destino, visitados + origen)
        subitinerarios.map(subitinerario => vuelo :: subitinerario)
      }
    }

    (cod1: String, cod2: String) => {
      val itinerarios = buscarItinerarios(cod1, cod2)
      itinerarios.sortBy(calcularTiempoTotal).take(3)
    }
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
      if (origen == destino) return List(List())
      if (visitados.contains(origen)) return List()

      val vuelosDesdeOrigen = vuelos.filter(_.Org == origen)
      vuelosDesdeOrigen.flatMap { vuelo =>
        val subitinerarios = buscarItinerarios(vuelo.Dst, destino, visitados + origen)
        subitinerarios.map(subitinerario => vuelo :: subitinerario)
      }
    }

    (cod1: String, cod2: String) => {
      val itinerarios = buscarItinerarios(cod1, cod2)
      itinerarios.sortBy(_.length).take(3)
    }
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    itinerariosTiempo(vuelos, aeropuertos)
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]] = {
    val mapaAeropuertos = aeropuertos.map(a => a.Cod -> a).toMap

    def calcularTiempoVuelo(vuelo: Vuelo): Int = {
      val origen = mapaAeropuertos(vuelo.Org)
      val destino = mapaAeropuertos(vuelo.Dst)
      val horaSalidaGMT = vuelo.HS * 60 + vuelo.MS + (origen.GMT * 60).toInt
      val horaLlegadaGMT = vuelo.HL * 60 + vuelo.ML + (destino.GMT * 60).toInt
      horaLlegadaGMT - horaSalidaGMT
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario.map(calcularTiempoVuelo).sum
    }

    def buscarItinerarios(origen: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
      if (origen == destino) return List(List())
      if (visitados.contains(origen)) return List()

      val vuelosDesdeOrigen = vuelos.filter(_.Org == origen)
      vuelosDesdeOrigen.flatMap { vuelo =>
        val subitinerarios = buscarItinerarios(vuelo.Dst, destino, visitados + origen)
        subitinerarios.map(subitinerario => vuelo :: subitinerario)
      }
    }

    (cod1: String, cod2: String, HC: Int, MC: Int) => {
      val itinerarios = buscarItinerarios(cod1, cod2)
      val horaCita = HC * 60 + MC
      itinerarios.filter { itinerario =>
        val tiempoTotal = calcularTiempoTotal(itinerario)
        tiempoTotal <= horaCita
      }.take(3)
    }
  }
}

