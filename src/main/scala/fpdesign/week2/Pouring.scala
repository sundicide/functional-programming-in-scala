package fpdesign.week2

class Pouring(capicity: Vector[Int]) {

 // States
 type State = Vector[Int]
 val initialState = capicity map (x => 0)

 // Moves
 trait Move {
   def change(state: State): State
 }
 case class Empty(glass: Int) extends Move {
   def change(state: State) = state updated (glass, 0)
 }
 case class Fill(glass: Int) extends Move {
   def change(state: State) = state updated (glass, capicity(glass))
 }
 case class Pour(from: Int, to: Int) extends Move {
   def change(state: State) = {
     val amount = state(from) min (capicity(to) - state(to))
     state updated (from, state(from) - amount) updated (to, state(to) + amount)
   }
 }

 val glasses = 0 until capicity.length

  // 가능한 모든 경우의 수
  // input이 new Pouring(Vector(4, 7))일 경우
  // Empty(0), Empty(1), Fill(0), Fill(1), Pour(0, 1), Pour(1, 0)
 val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths
   class Path(history: List[Move], val endState: State) {
//    def endState: State = (history foldRight initialState) (_ change _)

//    def endState: State = trackState(history)
//    private def trackState(xs: List[Move]): State = xs match {
//      case Nil => initialState
//      case move :: xs1 => move change trackState(xs1)
//    }

    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ") + " --> " + endState
  }

   val initialPath = new Path(Nil, initialState)

   def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
     if (paths.isEmpty) Stream.empty
     else {
       val more = for {
         path <- paths
         next <- moves map path.extend
         if !(explored contains next.endState)
       } yield next
       paths #:: from(more, explored ++ (more map (_.endState)))
     }

   val pathSets = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}
object test {
  def main(args: Array[String]): Unit = {
    val problem = new Pouring(Vector(4, 9))

    println(problem.moves)

    println(problem.pathSets)

    println(problem.pathSets.take(3).toList)

    println(problem.solutions(6).toList)
  }
}
