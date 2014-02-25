/**
 * This program is written by John Lewis and Yupeng Lu.
 * Each of us did the same amount of work in the program.
 */


import scala.util.Random
import scala.collection.immutable.Vector

/**
 * Describes a single card in a standard Bridge deck.
 * @param value An integer from 1 (Ace) to 13 (King)
 * @param suit One of "Clubs", "Diamonds", "Hearts", "Spades"
 */
class Card(val value: Int, val suit: String) {
  /**
   * Returns a string that describes this Card.
   */
  override def toString =
    (value match {
      case 1 => "Ace"
      case 11 => "Jack"
      case 12 => "Queen"
      case 13 => "King"
      case _ => value.toString
    }) + " of " + suit
    
    def toShortString =
      (value match {
      case 1 => "A"
      case 11 => "J"
      case 12 => "Q"
      case 13 => "K"
      case _ => value.toString
    }) + suit(0)
}

/**
 * Describes a complete deck of 52 playing cards.
 */
object CardDeck {
  val cards = for (
    value <- 1 to 13;
    suit <- List("Clubs", "Diamonds", "Hearts", "Spades")
  ) yield new Card(value, suit)
  
  /**
   * Returns a random selection of 13 Cards from this CardDeck.
   */
  def deal = Random.shuffle(cards) take 13

}

object Bid {

  type Hand = scala.collection.immutable.IndexedSeq[Card]
 
  /**
   * Returns an Array[Int] with the number of cards in each suit
   * arranged from weakest to strongest suit.
   */
  def getSuitCounts(hand: Hand): Array[Int] = {
    var clubs = length("Clubs", hand)
    var diamonds = length("Diamonds", hand)
    var hearts = length("Hearts", hand)
    var spades = length("Spades", hand)
    var suitCounts = Array(clubs, diamonds, hearts, spades)
    suitCounts
  }

  /**
   * Checks whether hand is balanced.
   */  
  def isBalanced(hand: Hand): Boolean = {
    var suitCounts = getSuitCounts(hand)
    var balanced = true
    for (n <- suitCounts) {
      if (n < 2 || n > 4) {
        balanced = false
      }
    }
    if (suitCounts(0) == 5 || suitCounts(1) == 5 && suitCounts.contains(3,2)) {
      balanced = true
    }
    balanced
  }
  
   
  /** 
   *  Returns the number of cards in hand matching suit.
   */  
  def length(suit: String, hand: Hand): Int = {
    var suitLength = 0
    for (n <- 0 to 12) {
      if (hand(n).suit == suit) {
        suitLength += 1
      }
    }
    suitLength
  } 

  /**
   * Returns the value of the high card points.
   */
  def highCardPoints(hand: Hand): Int = {
    var points = 0
    for (n <- 0 to 12) {
      if (hand(n).value == 1) {
        points += 4
      } else if (hand(n).value == 13) {
        points += 3
      } else if (hand(n).value == 12) {
        points += 2
      } else if (hand(n).value == 11) {
        points += 1
      }
    }
    points
  }
  
  /**
   * Returns the value of the distribution points.
   */   
  def distributionPoints(hand: Hand): Int = {
    var points = 0
    var suitName = Array("Clubs", "Diamonds", "Hearts", "Spades")
    for (n <- suitName) {
      if (length(n, hand) > 4) {
        points += length(n, hand) - 4
      }
    }
    points
  }
  
  /**
   * Returns the value of the point count of a hand.
   */   
  def pointCount(hand: Hand): Int = {
    highCardPoints(hand) + distributionPoints(hand)
  }
 
  /**
   * Determines whether to bid 1NT.
   */
  def shouldBidOneNT(hand: Hand): Boolean = {
    var shouldBid = false
    val points = highCardPoints(hand)
    if (points >= 16 && points <= 18 && isBalanced(hand) == true) {
      shouldBid = true
    }
    shouldBid
  }
  
  /**
   * Determines whether to bid 1H or 1S. Output a string for
   * the following two methods.
   */
  def bidOneHOrOneS(hand: Hand): String = {
    val heartsLength = length("Hearts", hand)
    val spadesLength = length("Spades", hand)
    val points = pointCount(hand)
    var bid = "null"
    if (points >= 13 && points <= 21 &&
    (heartsLength >= 5 || spadesLength >= 5)) {
      if (heartsLength == spadesLength) {
        bid = "Spades"
      } else if (heartsLength > spadesLength) {
        bid = "Hearts"
      } else if (heartsLength < spadesLength) {
        bid = "Spades"
      }
    }
    bid
  }
  
  /**
   * Determines whether to bid 1H. Output a boolean.
   */
  def shouldBidOneH(hand: Hand): Boolean = {
    var shouldBid = false
    val oneHOrOneS = bidOneHOrOneS(hand)
    if (oneHOrOneS == "Hearts") {
      shouldBid = true
    }
    shouldBid
  }
  
  /**
   * Determines whether to bid 1S. Output a boolean.
   */	 
  def shouldBidOneS(hand: Hand): Boolean = {
    var shouldBid = false
    val oneHOrOneS = bidOneHOrOneS(hand)
    if (oneHOrOneS == "Spades") {
      shouldBid = true
    }
    shouldBid
  }
  
  /**
   * Determines whether to bid 1C or 1D. Output a string for
   * the following two methods.
   */  
  def bidOneCOrOneD(hand: Hand): String = {
    val clubsLength = length("Clubs", hand)
    val diamondsLength = length("Diamonds", hand)
    val points = highCardPoints(hand)
    var bid = "null"
    if (isBalanced(hand) && (points == 19 || points == 20)) {
      if (clubsLength == 3 && diamondsLength == 3) {
        bid = "Clubs"
      } else if (clubsLength == diamondsLength) {
        bid = "Diamonds"
      } else if (clubsLength > diamondsLength) {
        bid = "Clubs"
      } else if (clubsLength < diamondsLength) {
        bid = "Diamonds"
      }
    }
    bid
  }
  
  /**
   * Determines whether to bid 1C. Output a boolean.
   */		  
  def shouldBidOneC(hand: Hand): Boolean = {
    var shouldBid = false
    if (bidOneCOrOneD(hand) == "Clubs") {
      shouldBid = true
    }
    shouldBid
  }
  
  /**
   * Determines whether to bid 1D. Output a boolean.
   */	  
  def shouldBidOneD(hand: Hand): Boolean = {
    var shouldBid = false
    if (bidOneCOrOneD(hand) == "Diamonds") {
      shouldBid = true
    }
    shouldBid
  }
  
  /**
   * Determines whether to bid 1C or 1D. Output a string for
   * the following two methods.
   */  
  def bidOneCOrOneDSecondCase(hand: Hand): String = {
    val clubsLength = length("Clubs", hand)
    val diamondsLength = length("Diamonds", hand)
    val points = pointCount(hand)
    var bid = "null"
    if (points >= 13 && points <= 21) {
      if (clubsLength == 3 && diamondsLength == 3) {
        bid = "Clubs"
      } else if (clubsLength == diamondsLength) {
        bid = "Diamonds"
      } else if (clubsLength > diamondsLength) {
        bid = "Clubs"
      } else if (clubsLength < diamondsLength) {
        bid = "Diamonds"
      }
    }
    bid
  }
  
  /**
   * Determines whether to bid 1C. Output a boolean.
   */	  
  def shouldBidOneCSecondCase(hand: Hand): Boolean = {
    var shouldBid = false
    if (bidOneCOrOneDSecondCase(hand) == "Clubs") {
      shouldBid = true
    }
    shouldBid
  }
  
  /**
   * Determines whether to bid 1D. Output a boolean.
   */	  
  def shouldBidOneDSecondCase(hand: Hand): Boolean = {
    var shouldBid = false
    if (bidOneCOrOneDSecondCase(hand) == "Diamonds") {
      shouldBid = true
    }
    shouldBid
  }
            
  /**
   * Returns the high card points for a specific suit.
   */  
  def getSuitPoints(suit: String, hand: Hand): Int = {
    var cardsInSuit = Array[Int]()
    var points = 0
    for (n <- 0 to 12) {
      if(hand(n).suit == suit) {
        cardsInSuit :+ hand(n).value
      }
    }
    for (n <- cardsInSuit) {
      if (n == 1) {
        points += 4
      } else if (n == 13) {
        points += 3
      } else if (n == 12) {
        points += 2
      } else if (n == 11) {
        points += 1
      }
    }
    points
  }
  
  /**
   * Determines whether to bid two of a suit.
   */   
  def shouldBidTwoOfASuit(hand: Hand, suitCounts: Array[Int]): Boolean = {
    var atLeastFiveInOneSuit = false
    for (n <- suitCounts) {
      if (n > 4) {
        atLeastFiveInOneSuit = true
      }
    }
    if (highCardPoints(hand) > 21 && atLeastFiveInOneSuit) {
      true
    } else {
      false
    }
  }
  
  /**
   * Determines which suit to bid two of in the case that shouldBidTwoOfASuit
   * is true.
   */   
  def getSuitToBidTwoOf(hand: Hand, suitCounts: Array[Int]): String = {
    var bid = ""
    if (suitCounts(3) > 4 && suitCounts(2) < 5) {
    	bid = "2S"
    } else if (suitCounts(2) > 4 && suitCounts(3) < 5) {
    	bid = "2H"
    } else if (suitCounts(2) > 5 && suitCounts(3) > 5) {
    	if (getSuitPoints("Hearts", hand) > getSuitPoints("Spades", hand)) {
    		bid = "2H"
    	} else if (getSuitPoints("Spades", hand) > getSuitPoints("Hearts", hand)) {
    		bid = "2S"
    	} else {
    		if (length("Hearts", hand) > length("Spades", hand)) {
    			bid = "2H"
    		} else if (length("Spades", hand) > length("Hearts", hand)) {
    			bid = "2S"
    		} else {
    			bid = "2H"
    		}
    	}
    } else if (suitCounts(0) > 5 && suitCounts(1) > 5) {
    	if (getSuitPoints("Clubs", hand) > getSuitPoints("Diamonds", hand)) {
    		bid = "2C"
    	} else if (getSuitPoints("Diamonds", hand) > getSuitPoints("Clubs", hand)) {
    		bid = "2D"
    	} else {
    		if (length("Clubs", hand) > length("Diamonds", hand)) {
    			bid = "2C"
    		} else if (length("Diamonds", hand) > length("Clubs", hand)) {
    			bid = "2D"
    		} else {
    			bid = "2C"
    		}
    	}
    } else {
    	if (suitCounts(0) > 4) {
    		bid = "2C"
    	} else {
    		bid = "2D"
    	}
    }
    bid
  }
  
  /**
   * Determines whether to bid three of a suit.
   */   
  def shouldBidThreeOfASuit(hand: Hand, suitCounts: Array[Int]): Boolean = {
    val points = highCardPoints(hand)
    if (points > 4 && points < 10 && suitCounts.contains(7)) {
      true
    } else {
      false
    }
  }
  
  /**
   * Determines which suit to bid three of in the case that
   * shouldBidThreeOfASuit is true.
   */   
  def getSuitToBidThreeOf(hand: Hand, suitCounts: Array[Int]): String = {
    var bid = ""
    var n = 0
    var go = true
    while(go && n < 4) {
      if (suitCounts(n) == 7) {
        go = false
      } else {
        n += 1
      }
    }
    n match {
      case 0 => (bid = "3C")
      case 1 => (bid = "3D")
      case 2 => (bid = "3H")
      case 3 => (bid = "3S")
    }
    bid
  }

  /**
   * Determines whether to bid four of a suit.
   */
  def shouldBidFourOfASuit(hand: Hand, suitCounts: Array[Int]): Boolean = {
    val points = highCardPoints(hand)
    if (points > 5 && points < 9 && suitCounts.contains(8)) {
      true
    } else {
      false
    }
  }
  
  /**
   * Determines which suit to bid four of in the case that
   * shouldBidFourOfASuit is true.
   */
  def getSuitToBidFourOf(hand: Hand, suitCounts: Array[Int]): String = {
    var bid = ""
    var n = 0
    var go = true
    while(go && n < 4) {
      if (suitCounts(n) == 8) {
        go = false
      } else {
        n += 1
      }
    }
    n match {
      case 0 => (bid = "4C")
      case 1 => (bid = "4D")
      case 2 => (bid = "4H")
      case 3 => (bid = "4S")
    }
    bid
  }
  
  /**
   * the main method is for making a new hand
   * and then return the bidding result by calling method bid
   */   
  def main(args: Array[String]) {
    val hand: Hand = CardDeck.deal
    bid(hand)
  }
  
  /**
   * method for determine which is the bid for a specific hand
   */
  def bid(hand: Hand): String = {
    var bid = "Pass"
	  var suitCounts = getSuitCounts(hand)
    if (shouldBidOneNT(hand)) {
        bid = "1NT"
    } else if (shouldBidOneH(hand)) {
        bid = "1H"
    } else if (shouldBidOneS(hand)) {
        bid = "1S"
    } else if (shouldBidOneC(hand)) {
        bid = "1C"
    } else if (shouldBidOneD(hand)) {
        bid = "1D"
    } else if (shouldBidOneCSecondCase(hand)) {
        bid = "1C"
    } else if (shouldBidOneDSecondCase(hand)) {
        bid = "1D"
    } else if (shouldBidTwoOfASuit(hand, suitCounts)) {
      bid = getSuitToBidTwoOf(hand, suitCounts)
    } else if (shouldBidThreeOfASuit(hand, suitCounts)) {
      bid = getSuitToBidThreeOf(hand, suitCounts)
    } else if (shouldBidFourOfASuit(hand, suitCounts)) {
      bid = getSuitToBidFourOf(hand, suitCounts)
    }
    bid
  }
}
