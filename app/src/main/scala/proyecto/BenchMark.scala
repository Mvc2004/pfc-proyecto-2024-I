package proyecto

import org.scalameter
import org.scalameter.{Quantity, Warmer, measure, withWarmer}

class BenchMark {
  def medirItinerarios(itinerarioSec: (String, String) => List[List[Vuelo]], itinerarioPar: (String, String) => List[List[Vuelo]]): (Quantity[Double], Quantity[Double]) = {
    val secuencial = withWarmer(new Warmer.Default) measure {
      itinerarioSec("DFW", "ATL")
    }

    val paralelo = withWarmer(new Warmer.Default) measure {
      itinerarioPar("DFW", "ATL")
    }

    (secuencial, paralelo)
  }

  def medirItinerariosSalida(itinerarioSec: (String, String, Int, Int) => List[Vuelo], itinerarioPar: (String, String, Int, Int) => List[Vuelo]): (Quantity[Double], Quantity[Double]) = {
    val secuencial = withWarmer(new Warmer.Default) measure {
      itinerarioSec("DFW", "ATL", 16, 5)
    }

    val paralelo = withWarmer(new Warmer.Default) measure {
      itinerarioPar("DFW", "ATL", 16, 5)
    }

    (secuencial, paralelo)
  }

  def mostrarResultado(secuencial: Quantity[Double], paralelo: Quantity[Double], nombreFuncion: String): String = {
    val aceleracion = secuencial.value / paralelo.value
    s"Tiempo de $nombreFuncion: Secuencial: $secuencial, Paralelo: $paralelo, Aceleración: $aceleracion"
  }
}