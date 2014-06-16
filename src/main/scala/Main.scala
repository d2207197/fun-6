import scala.collection.immutable.SortedSet

import scala.Console._
class Bridge(timeIntervals: Vector[Int]) {

  object Location extends Enumeration {
    type Location = Value
    val Here, There = Value
  }
  import Location._

  case class State(people: Vector[Location], light: Location, duration: Int) {
    def updated(person: Int, loc: Location): State =
      updated(Seq(person), loc)
    def updated(persons: Seq[Int], loc: Location): State = {
      if (((persons map people) :+ light) exists (_ == loc))
        this
      else
        State(
          persons.foldLeft(people) { (_people, person) => _people updated (person, loc) },
          loc,
          (persons map timeIntervals).max + this.duration)
    }
  }

  val initialState = State(timeIntervals map (x => Here), Here, 0)

  trait Move { def change(state: State): State }

  case class Go(person: Int) extends Move {
    def change(state: State): State =
      state updated (person, There)
  }

  case class Go2(person1: Int, person2: Int) extends Move {
    def change(state: State): State =
      state updated (Seq(person1, person2), There)
  }

  case class Back(person: Int) extends Move {
    def change(state: State): State =
      state updated (person, Here)
  }

  case class Back2(person1: Int, person2: Int) extends Move {
    def change(state: State): State =
      state updated (Seq(person1, person2), Here)
  }

  val people = Vector.range(0, timeIntervals.length)
  val moves =
    ( (for (p <- people) yield Go(p)) ++
      (for (p <- people) yield Back(p)) ++
      (for {p1 <- people
        p2 <- people
        if p1 < p2} yield Go2(p1, p2)))

  class Path(val history: List[Move], val endState: State) extends Ordered[Path] {
    def extend(move: Move) = {
      // println( new Path(move :: history, move change endState))
      new Path(move :: history, move change endState)
    }
    override def toString =
      (history.reverse mkString " ") + s" -->$YELLOW $endState $RESET"
    def compare(that: Path) =
      endState.duration compare that.endState.duration match {
        case x @ (1 | -1) => x
        case _ =>
          endState == that.endState match {
            case false => -1
            case true =>
              history == that.history match {
                case false => -1
                case true => 0
              }
          }
      }
  }

  val initialPath = new Path(Nil, initialState)
  def from(paths: SortedSet[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  }

  val pathSets = from(SortedSet(initialPath), Set(initialState))
  def solution: Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet.toList
      if (path.endState.people forall (_ == There))
    } yield path
}

object Main extends App {
  val problem = new Bridge(Vector(1, 2, 5, 10))
  problem.solution take 2 foreach println
}
