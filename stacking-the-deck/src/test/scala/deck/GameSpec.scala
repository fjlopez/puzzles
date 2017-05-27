package deck

import org.scalatest._
import org.scalatest.concurrent._
import org.scalatest.time.SpanSugar._
import org.scalatest.concurrent.Timeouts._

import Game._

class GameSpec extends FlatSpec with Matchers with TimeLimits {

  val winnerDec = toDeck("3 4 5 6 7 8 7 6 5 4 3 2 1 2 8 1")

  val loserDeck = toDeck("3 5 4 6 7 6 5 1 8 4 4 2 7 2 1 8")
  
  "A Deck object" should "contain 16 cards consisting of the ace through 8 of hearts and the ace through the 8 of spades" in {
    winnerDec.valid shouldBe true
  }

  "A player" should "win if ends with the final card and is an ace" in {
    winnerDec.play(1) shouldBe Win
  }

  "A player" should "lose if ends with the final card and is not an ace" in {
    loserDeck.play(1) shouldBe Lose(1, 8, Nil)
  }

  "A player" should "lose if the number of revealed cards is more than the remaining cards" in {
    loserDeck.play(8) shouldBe Lose(8, 8, toList("4 4 2 7 2 1 8"))
  }

  "A player" should "be able to find an arrangement in which he can always win" in {
    winnerDec.fullPlay() shouldBe Win
  }

  "A player" should "not win if it exists a number between 1 and 8 that cause the opponent win" in {
    loserDeck.fullPlay() shouldBe Multiresult(List(
      Lose(1, 8, Nil), Lose(2, 8, Nil), Lose(3, 8, Nil), Lose(4, 8, Nil),
      Lose(5, 8, Nil), Lose(6, 8, Nil), Lose(7, 8, Nil), Lose(8, 8, toList("4 4 2 7 2 1 8"))))
  }

  "A full deck" should "not have children" in {
    winnerDec.children shouldBe Nil
  }

  "A deck with only one ace" should "have 15 children" in {
    toDeck("1").children.size shouldBe 15
  }

  "A problem deck" should "have a winner solution" in {
    ProblemDeck.find() shouldBe defined
  }

  "A problem deck" should "with a prearranged cards should find a winner solution" in {
    ProblemDeck.find(List(toDeck("", "5 2 2 3 3 4 4 1"))) shouldBe Some(toDeck("6 6 8 7 8 7 1 5 5 2 2 3 3 4 4 1"))
  }

  "A problem deck" should "with a prearranged cards should find a winner solution fast" in {
    failAfter(1 second) {
       ProblemDeck.find(List(toDeck("", "8 6 5 7 8 6 3 7"))) 
    } (new ProblemDeck.BacktrackingSignaler)
  }
}
