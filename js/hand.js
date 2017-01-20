var Clubs = {
	value : 20,
	first : 0
};

var Diamonds = {
	value : 20,
	first : 0
};

var Hearts = {
	value : 30,
	first : 0
};

var Spades = {
	value : 30,
	first : 0
};

var NoTrumps = {
	value : 30,
	first : 10
};

function Hand() {
	// Default Values
	this.suit = null; // Suit bid [Clubs, Diamonds, Hearts, Spades, NoTrumps]
	this.level = null; // Level bid [1..7]
	this.declarer = null; // Declarer
	this.vulnerable = false; // Vulnerability of declarer [true, false]
	this.result = null; // Tricks won by declarer [0..13], null means result was got exactly
	this.honours = null; // Honours held out of 5 for trump suit, or all aces held [-5, -4, null, 4, 5] Negative indicates opposition holding honours
	this.victory = true; // Whether or not the contract was made
	this.scoreBelow = null; // Score below the line for declarer [0..880]
	this.scoreAbove = null; // Score above the line for declarer [0..4150]
	this.scoreOpponent = null; // Score above the line for opposition [0..7750]
	this.double = 1; // The multiplier for this hand [1, 2, 4]

	this.score = function() { // Score the hand based on set variables, return true if successful
		if (this.suit == null || this.level == null) { // When hand entry is invalid
			this.scoreBelow = null;
			this.scoreAbove = null;
			this.scoreOpponent = null;
			return false;
		}
		this.scoreAbove = 0;
		this.scoreBelow = 0;
		this.scoreOpponent = 0;
		this.victory = (this.result == null || this.result > this.level + 5);
		if (this.victory) { // When hand was won
			this.scoreBelow = (this.suit.value * this.level + this.suit.first) * this.double; // Score below line
			if (this.result > this.level + 6) { // Score overtricks
				if (this.double == 1) {
					this.scoreAbove = this.suit.value * (this.result - 6 - this.level);
				}
				else {
					this.scoreAbove = (this.vulnerable + 1) * 50 * this.double * (this.result - 6 - this.level);
				}
			}
			if (this.double != 1) { // Insult bonus
				this.scoreAbove += 25 * this.double;
			}
			if (this.level > 5) { // Slams
				this.scoreAbove += (500 + this.vulnerable * 250) * (this.level - 5);
			}
		}
		else { // When hand was lost
			this.scoreOpponent = 50 * (this.level - this.result + 6) * (this.vulnerable + 1) * this.double;
			if (this.level - this.result > -5 && this.double != 1) {
				this.scoreOpponent += (50 * (this.level - this.result + 5) * this.double);
				if (this.level - this.result > -3 && !this.vulnerable) {
					this.scoreOpponent += (50 * (this.level - this.result + 3) * this.double);
				}
			}
		}
		if (this.honours != null) { // Honours bonuses
			if (this.suit == NoTrumps && (this.honours == 4 || this.honours == -4)) {
				this.honours = this.honours / 4 * 5;
			}
			if (this.honours > 0) { // Declarer has honours
				this.scoreAbove += 50 * (this.honours - 2);
			}
			else { // Opposition has honours
				this.scoreOpponent += 50 * (this.honours - 2);
			}
		}
		return true;
	}
}
