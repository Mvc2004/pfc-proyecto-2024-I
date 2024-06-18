package proyecto

case class Aeropuerto(codigo: String, latitud: Int, longitud: Int, zonaHoraria: Int)

case class Vuelo(

                  aerolinea: String,
                  numero: Int,
                  origen: String,
                  horaSalida: Int,
                  minutoSalida: Int,
                  destino: String,
                  horaLlegada: Int,
                  minutoLlegada: Int,
                  dias: Int
                )