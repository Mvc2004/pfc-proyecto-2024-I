package proyecto

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

class ItinerarioManager {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => Future[List[Itinerario]] = {
    (cod1: String, cod2: String) => {
      Future {
        buscarItinerarios(cod1, cod2, vuelos)
      }
    }
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => Future[List[Itinerario]] = {
    (cod1: String, cod2: String) => {
      Future {
        buscarItinerarios(cod1, cod2, vuelos).sortBy(_.tiempoTotal).take(3)
      }
    }
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => Future[List[Itinerario]] = {
    (cod1: String, cod2: String) => {
      Future {
        buscarItinerarios(cod1, cod2, vuelos).sortBy(_.numeroEscalas).take(3)
      }
    }
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => Future[List[Itinerario]] = {
    (cod1: String, cod2: String) => {
      Future {
        buscarItinerarios(cod1, cod2, vuelos).sortBy(_.tiempoEnAire).take(3)
      }
    }
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Future[List[Itinerario]] = {
    (cod1: String, cod2: String, HC: Int, MC: Int) => {
      Future {
        val horaCita = Utils.convertirTiempo(HC, MC)
        buscarItinerarios(cod1, cod2, vuelos)
          .filter(_.vuelos.lastOption.exists(v => Utils.convertirTiempo(v.HL, v.ML) <= horaCita))
          .sortBy(it => Utils.convertirTiempo(it.vuelos.head.HS, it.vuelos.head.MS))
          .take(3)
      }
    }
  }

  private def buscarItinerarios(origen: String, destino: String, vuelos: List[Vuelo]): List[Itinerario] = {
    def buscar(actual: String, destino: String, visitados: Set[String], camino: List[Vuelo]): List[List[Vuelo]] = {
      if (actual == destino) List(camino)
      else {
        vuelos.filter(v => v.Org == actual && !visitados.contains(v.Dst)).flatMap { vuelo =>
          buscar(vuelo.Dst, destino, visitados + actual, camino :+ vuelo)
        }
      }
    }

    buscar(origen, destino, Set(), List()).map(Itinerario)
  }
}

