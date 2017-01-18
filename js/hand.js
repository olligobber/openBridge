function Hand() {
	// Default Values
	this.suit = null; // Suit bid ["Clubs", "Diamonds", "Hearts", "Spades", "No Trumps"]
	this.level = null; // Level bid [1..7]
	this.declarer = null; // Declarer (Any string)
	this.vulnerable = false; // Vulnerability of declarer [true, false]
	this.result = null; // Tricks won by declarer [0..13]
	this.honours = null; // Honours held [-5, -4, null, 4, 5] Negative indicates opposition holding honours
	this.victory = true; // Whether or not the contract was made
	this.scoreBelow = null; // Score below the line for declarer [0..880]
	this.scoreAbove = null; // Score above the line for declarer [0..4150]
	this.scoreOpponent = null; // Score above the line for opposition [0..7750]
	this.double = 1; // The multiplier for this hand [1, 2, 4]

	this.score = function() {
		if (this.suit == null || this.level == null) {
			this.scoreBelow = null;
			this.scoreAbove = null;
			this.scoreOpponent = null;
			return;
		}
		this.scoreAbove = 0;
		this.scoreBelow = 0;
		this.scoreOpponent = 0;
		this.victory = (this.result == null || this.result > this.level + 5);
		if (this.victory) {
			if (this.suit == "Clubs" || this.suit == "Diamonds") {
				this.scoreBelow = 20 * this.level * double;
			}
			else if (this.suit == "Hearts" || this.suit == "Spades") {
				this.scoreBelow = 30 * this.level * double;
			}
			else {
				this.scoreBelow = (30 * this.level + 10) * double;
			}
			if (this.result > this.level + 6) {
				if (this.double == 1) {
					if (this.suit == "Clubs" || this.suit == "Diamonds") {
						this.scoreAbove = 20 * (this.result - 6 - this.level);
					}
					else {
						this.scoreAbove = 30 * (this.result - 6 - this.level);
					}
				}
				else {
					this.scoreAbove = (this.vulnerable + 1) * 50 * this.double * (this.result - 6 - this.level);
				}
			}
			if (this.double != 1) {
				this.scoreAbove += 25 * this.double;
			}
			if (this.level == 12) {
				if (this.vulnerable) {
					this.scoreAbove += 750;
				}
				else {
					this.scoreAbove += 500;
				}
			}
			else if (this.level == 13) {
				if (this.vulnerable) {
					this.scoreAbove += 1500;
				}
				else {
					this.scoreAbove += 1000;
				}
			}
		}
	}
}
