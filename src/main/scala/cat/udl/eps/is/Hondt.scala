package cat.udl.eps.is

import scala.List

// Eleccions Parlament Catalunya 2021 (Lleida)
// https://www.parlament.cat/pcat/parlament/que-es-el-parlament/procediment-electoral/
// https://gencat.cat/eleccions/resultatsparlament2021/resultados/3/catalunya/lleida

// Consulteu la llista de mètodes a StdLib

object Hondt {

  // El paràmetres d'entrada són
  // - els vots que ha rebut cada partit (només els que han passat el tall del 5%)
  // - el nombre d'escons a repartir
  // La sortida ha de ser, per cada partit, el nombre d'escons que li pertoquen

  // Recordeu que dins d'una funció podeu fer servir `val` per guardar resultats intermedis
  // i crear funcions auxiliars (si calen)

  def hondt(votes: Map[String, Int], n: Int): Map[String, Int] = 
    // Ordena los partidos de mayor a menor número de votos
    val orderedParties = votes.toList.sortBy(-_._2).map(_._1)

    // Función auxiliar que calcula los escaños que obtiene un partido
    def calculateSeats(parties: String, divisor: Int): Int = votes(parties) / divisor

    // Función recursiva que distribuye los escaños
    def distributeSeats(seats: Map[String, Int], divisor: Int, r: Int): Map[String, Int] = 
      if (r == 0) seats
      else {
        // Calcula los nuevos escaños que obtienen los partidos
        val newSeats = orderedParties.map(party => (party, calculateSeats(party, divisor)))

        // Encuentra el partido que más escaños ha obtenido
        val winner = newSeats.foldLeft((orderedParties.head, 0)) { (acc, cur) =>
          if (cur._2 > acc._2) cur else acc
        }._1

        // Añade el partido ganador al mapa de escaños
        val newSeatCount = seats.getOrElse(winner, 0) + 1
        distributeSeats(seats + (winner -> newSeatCount), divisor + 1, r - 1)
      }

    distributeSeats(Map.empty[String, Int], 1, n)
  
}
