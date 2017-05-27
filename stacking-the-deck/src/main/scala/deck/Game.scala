package deck

object Game extends App {

  class Card
  object Jocker extends Card
  case class Face(value: Int) extends Card

  class Result
  case object Win extends Result
  case class Lose(pos: Int, value: Int, remain: Seq[Card]) extends Result
  case class Multiresult(result: Seq[Result]) extends Result

  case class Deck(cards: List[Card], opponent: List[Card] = List()) {
    lazy val remain: List[Card] = (1 to 8).toList.flatMap(value => List.fill(2 - (cards ::: opponent).count { case Face(v) => v == value })(Face(value)))
    
    def valid: Boolean = cards.size == 16 && cards.groupBy(card => card match {
      case Face(value) => value
      case _           => 0
    }).mapValues(_.size).forall {
      case (k, v) => v == 2 && k >= 1 && k <= 8
    }

    def play(pos: Int): Result = {
      def play(n: Int, inplay: Seq[Card]): Result = inplay drop n - 1 match {
        case Face(value) :: Nil if value == 1             => Win
        case Face(value) :: remain if value > remain.size => Lose(pos, value, remain)
        case Face(value) :: remain                        => play(value, remain)
      }
      play(pos, cards)
    }

    def fullPlay(limit: Int = 8): Result = {
      (1 to limit) map (play(_)) filter (_ != Win) toList match {
        case Nil => Win
        case x   => Multiresult(x)
      }
    }

    def children: List[Deck] = {
      val result = remain.map(card => Deck(card :: cards, opponent))
      if (opponent.isEmpty) result
      else Deck(opponent.last :: cards, opponent.init) :: result
    }
  }

  def toDeck(deck: String, opponent: String = ""): Deck = Deck(toList(deck), toList(opponent))
  def toList(deck: String): List[Card] = deck.split(" ").toList.filter(_ != "").map(x => Face(x.toInt))

  object ProblemDeck extends Backtracking {
    type Node = Deck

    def root = toDeck("1")

    def analyse(node: Deck) = {
      if (node.cards.size > 8 && node.fullPlay(8 - node.cards.size) != Win) Rejected
      else if (node.cards.size == 16 && node.fullPlay() == Win) Accepted
      else Undecided(node.children)
    }
  }
}