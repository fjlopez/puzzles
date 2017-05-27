package deck

import scala.annotation.tailrec
import com.typesafe.scalalogging.Logger
import org.scalatest.concurrent.Signaler

trait Backtracking {
  type Node

  trait Result
  case object Rejected extends Result
  case object Accepted extends Result
  case class Undecided(children: List[Node]) extends Result

  val logger = Logger("name")
  
  def root: Node

  def analyse(node: Node): Result

  @volatile var interrupt = false
  
  @tailrec
  final def find(candidates: List[Node] = List(root)): Option[Node] = {
    if (interrupt) {
      logger.error(s"Interrupted with ${candidates.size} candidates in queue")
      throw new Exception("Interrupted")
    }
    candidates match {
      case candidate :: tail =>
      analyse(candidate) match {
        case Accepted => 
          logger.info(s"Found : $candidate")
          Some(candidate)
        case Rejected =>
          logger.info(s"Rejected : $candidate")
          find(tail)
        case Undecided(children) =>
          logger.info(s"Undecided with ${children.size} children: $candidate")
          find(children ::: tail)
      }
    case Nil => None
    }
  }

  class BacktrackingSignaler extends Signaler {
    def apply(thr: Thread) { interrupt = true }
  }
}