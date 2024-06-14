package proyecto

object Utils {
  def convertirTiempo(horas: Int, minutos: Int): Int = horas * 60 + minutos

  def tiempoVuelo(vuelo: Vuelo): Int = {
    val tiempoSalida = convertirTiempo(vuelo.HS, vuelo.MS)
    val tiempoLlegada = convertirTiempo(vuelo.HL, vuelo.ML)
    tiempoLlegada - tiempoSalida
  }
}

