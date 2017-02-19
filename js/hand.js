/*
	Copyright 2017 olligobber

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

		http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
*/

/*
	This project provides a scorepad for rubber bridge, intended to be used on
	any device, and is currently hosted at

		https://github.com/olligobber/openBridge

	Go to the github page to report any bugs and contribute to the project.

	This script creates a scaffold for scoring individual hands in rubber
	bridge, and is intended to be as versatile as possible for other apps, not
	just the latest version of the openBridge scorepad.
*/

var hand_js_version = "1.1.1" // To avoid cache issues, this can be checked by other files that use hand.js


/*
	Contains five objects for each biddable suit, so they can be compared to
	each other, and scored. The value for each level bid and each overtrick
	is stored in the "value" property of each suit, and the bonus for the first
	level bid, relevant only to No Trumps, is stored in the "first" property of
	each suit.
*/
var suit = {

	Clubs : {
		value : 20,
		first : 0
	},

	Diamonds : {
		value : 20,
		first : 0
	},

	Hearts : {
		value : 30,
		first : 0
	},

	Spades : {
		value : 30,
		first : 0
	},

	NoTrumps : {
		value : 30,
		first : 10 // Since the first level bid is worth 40 and subsequent levels only 30
	}

};

/*
	Constructs an object for storing and scoring a hand in rubber bridge.

	Intended use is using the following code:

		var myHand = new Hand();
*/
function Hand() {
	// Default Values
	this.suit = null; // Suit bid (Any suit object)
	this.level = null; // Level bid [1..7]
	this.vulnerable = false; // Vulnerability of declarer (boolean)
	this.result = null; // Tricks won by declarer [0..13], null means result was got exactly, with no undertricks or overtricks
	this.honours = 0; // Honours held, out of 5 in the trump suit, or aces held in no trumps [-5, -4, 0, 4, 5], negative indicates opposition holding honours
	this.victory = true; // Whether or not the contract was made
	this.scoreBelow = null; // Score below the line for declarer [0..880]
	this.scoreAbove = null; // Score above the line for declarer [0..4150]
	this.scoreOpponent = null; // Score above the line for opposition [0..7750]
	this.double = 1; // The multiplier for this hand [1, 2, 4]

	/*
		Updates the score values.
		Returns false if hand is not valid and cannot be scored.
		Returns true if hand is valid and was successfully scored.

		Does not score rubber bonuses.
	*/
	this.score = function() {
		// Check if hand is valid and can be scored
		if (this.suit == null || this.level == null) {
			this.scoreBelow = null;
			this.scoreAbove = null;
			this.scoreOpponent = null;
			return false;
		}

		// Reset scores
		this.scoreAbove = 0;
		this.scoreBelow = 0;
		this.scoreOpponent = 0;

		// Check if contract was made
		this.victory = (this.result == null || this.result > this.level + 5);

		// If contract was made
		if (this.victory) {

			// Score below the line
			this.scoreBelow = (this.suit.value * this.level + this.suit.first) * this.double;

			// Score overtricks
			if (this.result > this.level + 6) {
				if (this.double == 1) {
					this.scoreAbove = this.suit.value * (this.result - 6 - this.level);
				}
				else {
					this.scoreAbove = (this.vulnerable + 1) * 50 * this.double * (this.result - 6 - this.level);
				}
			}

			// Insult bonus
			if (this.double != 1) {
				this.scoreAbove += 25 * this.double;
			}

			// Slam bonuses
			if (this.level > 5) {
				this.scoreAbove += (500 + this.vulnerable * 250) * (this.level - 5);
			}
		}

		// When contract was not made
		else {

			// Base score for undertricks
			this.scoreOpponent = 50 * (this.level - this.result + 6) * (this.vulnerable + 1) * this.double;

			// 2nd and subsequent undertricks in doubled or redoubled contracts
			if (this.level - this.result > -5 && this.double != 1) {
				this.scoreOpponent += (50 * (this.level - this.result + 5) * this.double);

				// 4th and subsequent undertricks in doubled or redoubled contracts when not vulnerable
				if (this.level - this.result > -3 && !this.vulnerable) {
					this.scoreOpponent += (50 * (this.level - this.result + 3) * this.double);
				}
			}
		}

		// Honours bonuses
		if (this.honours != 0) {

			// Set honours bonus to be [-5, 0, 5] when in No Trumps
			if (this.suit == suit.NoTrumps && (this.honours == 4 || this.honours == -4)) {
				this.honours = this.honours / 4 * 5;
			}

			// Declarer has honours
			if (this.honours > 0) {
				this.scoreAbove += 50 * (this.honours - 2);
			}

			// Opposition has honours
			else {
				this.scoreOpponent += 50 * (-this.honours - 2);
			}
		}

		// Hand has been validated and scored
		return true;
	}
}
