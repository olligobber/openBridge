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

	This script provides a scorepad object which interacts with the buttons
	and elements in index.html, and relies heavily on hand.js for hand
	management and scoring.
*/

var main_js_version = "1.2" // To avoid cache issues, this can be checked by other files that use main.js

// Check that hand.js was loaded
if (typeof hand_js_version == "undefined") {
	console.error("main.js detected that hand.js was not loaded");
}

// Check that the most recent version of hand.js was loaded
if (hand_js_version != "1.1.1") {
	console.error("main.js detected that the current version of hand.js was not correctly loaded");
}

/*
	Add some extra properties to the suits in hand.js for each suit's symbol
	and their button in the hand editor
*/
suit.Clubs.symbol = '&clubs;';
suit.Clubs.element = document.getElementById('suit').children[0].children[0];
suit.Diamonds.symbol = '&diams;';
suit.Diamonds.element = document.getElementById('suit').children[1].children[0];
suit.Hearts.symbol = '&hearts;';
suit.Hearts.element = document.getElementById('suit').children[2].children[0];
suit.Spades.symbol = '&spades;';
suit.Spades.element = document.getElementById('suit').children[3].children[0];
suit.NoTrumps.symbol = '\\';
suit.NoTrumps.element = document.getElementById('suit').children[4].children[0];

/*
	Contains 4 objects for each of the players, a null player and a list of the
	player objects. Each player object contains the html element for their
	button in the hand editor, their symbol for the hand name, a longer name,
	their team, their opponent's team, a multiplier for their team when saving
	honours bonuses, and an index for their position in the list.
*/
var players = {

	North : {
		element : document.getElementById('declarer').children[0].children[0],
		symbol : 'N',
		name: 'North',
		team : 'we',
		opponent : 'they',
		multiplier : 1,
		index : 0
	},

	East : {
		element : document.getElementById('declarer').children[1].children[0],
		symbol : 'E',
		name: 'East',
		team : 'they',
		opponent : 'we',
		multiplier: -1,
		index : 1
	},

	South : {
		element : document.getElementById('declarer').children[2].children[0],
		symbol : 'S',
		name: 'South',
		team : 'we',
		opponent : 'they',
		multiplier : 1,
		index : 2
	},

	West : {
		element : document.getElementById('declarer').children[3].children[0],
		symbol : 'W',
		name: 'West',
		team : 'they',
		opponent : 'we',
		multiplier : -1,
		index : 3
	},

	None : {
		element : null,
		symbol : null,
		name : null,
		team : null,
		opponent : null,
		multiplier : 0,
		index : null
	}

};
players.list = [players.North, players.East, players.South, players.West];

// Elements of index.html for use throughout the rest of the script
var elements = {

	// The AP button in the hand editor
	allPass : document.getElementById('declarer').children[4].children[0],

	// A list of buttons for the level bid in the hand editor
	level : [null],

	// Result text in the hand editor
	result : document.getElementById('result'),

	// Entire hand editor area, so it can be shown and hidden as appropriate
	buttons : document.getElementById('buttons'),

	// Dropdown for selecting the hand multiplier
	double : document.getElementById('double'),

	// Dropdown for selecting honours bonuses
	honours : document.getElementById('honours'),

	// The tbody element that contains the hands in the scorepad
	history : document.getElementById('history').children[0].children[0],

	// Where points above the line are scored
	above : {
		we : document.getElementById("we_above"),
		they : document.getElementById("they_above")
	},

	// Scores below the line
	scores : document.getElementById("scorepad").children[0].children[1],

	// The element below the scores below the line
	botpad : document.getElementById("botpad"),

	// Where the totals for the scorepad are shown
	total : {
		we : document.getElementById("total").children[0],
		they : document.getElementById("total").children[1]
	},

	// Dropdown for selecting the first dealer
	firstDealer : document.getElementById("firstDealer"),

	// Where the current dealer is shown
	dealer : document.getElementById("dealer")

};

// Adds the buttons for levels bid to the list elements.level
for (var i=0; i<7; ++i) {
	elements.level.push(document.getElementById('level').children[i].children[0]);
}

// Returns a string representing the number of undertricks/overtricks in a hand
function renderResult(level, result) {
	if (result == null) {
		return "+0";
	}
	if (result > level + 5) {
		return "+" + (result - 6 - level);
	}
	return "" + (result - 6 - level);
}

/*
	Creates a html element of a given type that contains given elements

	Borrowed from http://eloquentjavascript.net/13_dom.html
*/
function elt(type) {
	var node = document.createElement(type);
	for (var i = 1; i < arguments.length; i++) {
		var child = arguments[i];
		if (typeof child == "string") {
			child = document.createTextNode(child);
		}
		node.appendChild(child);
	}
	return node;
}

// Returns a string that describes the hand bid and its result
Hand.prototype.render = function() {
	if (this.allPass) {
		return "AP";
	}
	if (this.declarer != players.None && this.score()) {
		var out = this.declarer.symbol + this.level + this.suit.symbol;
		if (this.double > 1) {
			out += "X";
		}
		else {
			out += "&nbsp;";
		}
		if (this.double > 3) {
			out += "X";
		}
		else {
			out += "&nbsp;";
		}
		out += renderResult(this.level, this.result);
		if (this.honours != 0) {
			out += " h";
		}
		return out;
	}
}

// Some added default values for hands
Hand.prototype.index = null; // The index of this hand (or its most recently submitted version) in scorepad.allHands
Hand.prototype.element = null; // The element for this hand on index.html
Hand.prototype.declarer = players.None; // The hand's declarer (Any player object)
Hand.prototype.allPass = false; // Whether all players passed or not

/*
	The scorepad element stores the variables associated with this scorepad,
	and the functions necessary to update it, accessed by buttons in index.html
*/
var scorepad = {
	currentHand : null, // The hand currently being edited
	allHands : [], // All hands submitted to the scorepad
	handCount : 0, // The number of non-null elements in allHands
	scoreElements : [], // The elements of scores below the line

	// Updates the current dealer
	updateDealer : function() {

		// No first dealer selected so clear the current dealer
		if (elements.firstDealer.value == -1) {
			elements.dealer.style.display = "none";
		}

		else {
			elements.dealer.style.display = "";
			elements.dealer.innerHTML = players.list[(parseInt(elements.firstDealer.value) + this.handCount) % 4].name + " will deal next";
		}
	},

	// Clears out any hand currently being edited, after or without submitting it
	clearHand : function() {
		elements.buttons.style.display = 'none'; // Hide the hand editor

		// If no hand was being edited
		if (this.currentHand == null) {
			return;
		}

		// If the hand being edited had never been submitted, delete its element
		if (this.currentHand.index == null) {
			this.currentHand.element.outerHTML = "";
		}
		// If the hand being edited had been submitted previously, change its element's appearance
		else {
			this.currentHand.element.className = "";
		}

		// Update the all pass button
		if (this.currentHand.allPass) {
			elements.allPass.className = "";
		}

		// Update the level, suit and declarer buttons
		else {
			if (this.currentHand.level != null) {
				elements.level[this.currentHand.level].className = "";
			}
			if (this.currentHand.suit != null) {
				this.currentHand.suit.element.className = "";
			}
			if (this.currentHand.declarer != players.None) {
				this.currentHand.declarer.element.className = "";
			}
		}

		elements.double.value = 1; // Reset the double dropdown
		elements.honours.value = 0; // Reset the honours dropdown
		elements.result.innerHTML = ""; // Clear the hand result
		this.currentHand = null; // Delete the unsubmitted hand
		// Minimise the History Div
		document.getElementById('history').style.height = '90%';
	},

	// Update the result element
	updateResult : function() {

		// If no hand is currently being edited
		if (this.currentHand == null) {
			elements.result.innerHTML = "";
		}

		// If the hand otherwise does not need a result
		else if (this.currentHand.allPass || this.currentHand.level == null) {
			elements.result.innerHTML = "";
		}

		// Render the result with appropriate spacing
		else if (this.currentHand.result == null) {
			if (this.currentHand.level < 4) {
				elements.result.innerHTML = "&nbsp;" + (this.currentHand.level+6) + "&nbsp;" + renderResult(this.currentHand.level, this.currentHand.result);
			}
			else {
				elements.result.innerHTML = (this.currentHand.level+6) + "&nbsp;" + renderResult(this.currentHand.level, this.currentHand.result);
			}
		}
		else {
			if (this.currentHand.result < 10) {
				elements.result.innerHTML = "&nbsp;" + (this.currentHand.result) + "&nbsp;" + renderResult(this.currentHand.level, this.currentHand.result);
			}
			else {
				elements.result.innerHTML = (this.currentHand.result) + "&nbsp;" + renderResult(this.currentHand.level, this.currentHand.result);
			}
		}
	},

	// Update the scorepad's scores
	updateScores : function() {

		// Clear out all scores in the scorepad
		elements.above.we.innerHTML = "";
		elements.above.they.innerHTML = "";
		while (this.scoreElements.length > 0) {
			this.scoreElements.pop().outerHTML = "";
		}

		// Total score by each team
		var totals = {
			we : 0,
			they : 0
		};

		// Total score below the line for each team
		var below = {
			we : 0,
			they : 0
		};

		// Number of games scored by each team in the current rubber
		var games = {
			we : 0,
			they : 0
		};

		for (var i = 0; i < this.allHands.length; ++i) {
			if (this.allHands[i] == null) {
				continue;
			}
			if (this.allHands[i].allPass) {
				continue;
			}

			// Apply vulnerability to the hand and update its score
			this.allHands[i].vulnerable = (games[this.allHands[i].declarer.team] == 1);
			this.allHands[i].score();

			// Place score below the line
			if (this.allHands[i].scoreBelow != 0) {
				totals[this.allHands[i].declarer.team] += this.allHands[i].scoreBelow;

				// Add a score below the line in the left column
				if (this.allHands[i].declarer.team == "we") {
					elements.scores.insertBefore(elt("tr", elt("td", "" + this.allHands[i].scoreBelow), elt("td")), elements.botpad);
				}

				// Add a score below the line in the right column
				else {
					elements.scores.insertBefore(elt("tr", elt("td"), elt("td", "" + this.allHands[i].scoreBelow)), elements.botpad);
				}

				this.scoreElements.push(elements.botpad.previousSibling); // Add the new score element to the list

				below[this.allHands[i].declarer.team] += this.allHands[i].scoreBelow;

				// A game has been achieved
				if (below[this.allHands[i].declarer.team] > 99) {
					elements.botpad.previousSibling.className = "game"; // Add a line below this score
					games[this.allHands[i].declarer.team]++;
					below.we = 0;
					below.they = 0;
				}
			}

			// Place a score above the line for declarer
			if (this.allHands[i].scoreAbove != 0) {
				totals[this.allHands[i].declarer.team] += this.allHands[i].scoreAbove;
				elements.above[this.allHands[i].declarer.team].innerHTML = this.allHands[i].scoreAbove + "<br>" + elements.above[this.allHands[i].declarer.team].innerHTML;
			}

			// Place a score above the line for opposition
			if (this.allHands[i].scoreOpponent != 0) {
				totals[this.allHands[i].declarer.opponent] += this.allHands[i].scoreOpponent;
				elements.above[this.allHands[i].declarer.opponent].innerHTML = this.allHands[i].scoreOpponent + "<br>" + elements.above[this.allHands[i].declarer.opponent].innerHTML;
			}

			// Add a rubber bonus above the line
			if (games[this.allHands[i].declarer.team] == 2) {

				// Rubber in 3
				if (games[this.allHands[i].declarer.opponent] == 1) {
					totals[this.allHands[i].declarer.team] += 500;
					elements.above[this.allHands[i].declarer.team].innerHTML = "500<br>" + elements.above[this.allHands[i].declarer.team].innerHTML;
				}

				// Rubber in 2
				else {
					totals[this.allHands[i].declarer.team] += 700;
					elements.above[this.allHands[i].declarer.team].innerHTML = "700<br>" + elements.above[this.allHands[i].declarer.team].innerHTML;
				}

				games.we = 0;
				games.they = 0;
			}

		}

		// Update totals
		elements.total.we.innerHTML = totals.we;
		elements.total.they.innerHTML = totals.they;

	},

	// Add a new hand and open the hand editor
	newHand : function() {
		this.clearHand();
		this.currentHand = new Hand()
		elements.history.insertBefore(elt("tr", elt("td"), elt("td", elt("button", "Edit")), elt("td", elt("button", "Delete"))), elements.history.children[2]); // Insert hand's element
		this.currentHand.element = elements.history.children[2];
		this.currentHand.element.className = 'selected';
		this.currentHand.element.getElementsByTagName('button')[0].setAttribute('type', 'button');
		this.currentHand.element.getElementsByTagName('button')[1].setAttribute('type', 'button');
		document.getElementById('history').style.height = '45%';
		elements.buttons.style.display = null; // Show hand editor
	},

	// Delete a hand from the scorepad
	removeHand : function(index) {
		this.allHands[index].element.outerHTML = ""; // Remove the element
		this.allHands[index] = null; // Remove the object
		this.updateScores();
		this.handCount--;

		// Clear the list if there are no hands left
		if (this.handCount == 0) {
			this.allHands = [];
		}

		this.updateDealer();
	},

	// Submit the hand currently being edited and close the hand editor
	submitHand : function() {

		// Check the hand is valid to be scored
		if (this.currentHand.allPass || this.currentHand.score()) {

			// Update values from dropdowns
			this.currentHand.double = parseInt(elements.double.value);
			this.currentHand.honours = this.currentHand.declarer.multiplier * parseInt(elements.honours.value);

			// Update hand's description
			this.currentHand.element.children[0].innerHTML = this.currentHand.render();

			// If this hand has never been submitted before, add it to the list and update its buttons' commands
			if (this.currentHand.index == null) {
				this.currentHand.index = this.allHands.length;
				this.allHands.push(this.currentHand);
				this.handCount++;
				this.currentHand.element.getElementsByTagName('button')[0].setAttribute('onClick', 'scorepad.editHand(' + this.currentHand.index + ');');
				this.currentHand.element.getElementsByTagName('button')[1].setAttribute('onClick', 'scorepad.removeHand(' + this.currentHand.index + ');');
			}
			else {
				this.allHands[this.currentHand.index] = this.currentHand;
			}
			document.getElementById('history').style.height = '90%';
			this.clearHand();
			this.updateScores();
			this.updateDealer();
		}

	},

	// Load a previously submitted hand and open the hand editor
	editHand : function(index) {
		this.clearHand();
		this.currentHand = Object.create(this.allHands[index]);
		this.currentHand.element.className = 'selected';

		// Select all pass button
		if (this.currentHand.allPass) {
			elements.allPass.className = 'selected';
		}

		// Select buttons for declarer, level and suit
		else {
			this.currentHand.declarer.element.className = 'selected';
			elements.level[this.currentHand.level].className = 'selected';
			this.currentHand.suit.element.className = 'selected';
		}

		// Update doubles and honours dropdowns
		elements.double.value = this.currentHand.double;
		elements.honours.value = this.currentHand.declarer.multiplier * this.currentHand.honours;

		this.updateResult();

		elements.buttons.style.display = null; // Show the hand editor buttons
	},

	// Set the all pass flag to true and update button selections accordingly
	allPassOn : function() {

		// If hand wasn't already an all pass hand
		if (!this.currentHand.allPass) {
			this.currentHand.allPass = true;
			elements.allPass.className = 'selected'; // Select the all pass button

			// Deselect other buttons
			if (this.currentHand.declarer != players.None) {
				this.currentHand.declarer.element.className = '';
			}
			if (this.currentHand.level != null) {
				elements.level[this.currentHand.level].className = '';
			}
			if (this.currentHand.suit != null) {
				this.currentHand.suit.element.className = '';
			}

			this.updateResult();
		}
	},

	// Set the all pass flag to false and update button selections accordingly
	allPassOff : function() {

		// If the hand was flagged as all pass
		if (this.currentHand.allPass) {
			this.currentHand.allPass = false;
			elements.allPass.className = ''; // Deslect the all psss button

			// Select buttons that were selected before all pass was turned on
			if (this.currentHand.declarer != players.None) {
				this.currentHand.declarer.element.className = 'selected';
			}
			if (this.currentHand.level != null) {
				elements.level[this.currentHand.level].className = 'selected';
			}
			if (this.currentHand.suit != null) {
				this.currentHand.suit.element.className = 'selected';
			}

			this.updateResult();
		}
	},

	// Set the declarer for this hand and update button selections accordingly
	setDeclarer : function(newDeclarer) {

		this.allPassOff(); // An all pass hand cannot have a declarer, so turn off all pass

		// Deselect the previous declarer
		if (this.currentHand.declarer != players.None) {
			this.currentHand.declarer.element.className = '';
		}

		this.currentHand.declarer = newDeclarer;
		newDeclarer.element.className = 'selected'; // Select the button for this declarer
	},

	// Set the suit for this hand and update button selections accordingly
	setSuit : function(newSuit) {

		this.allPassOff(); // An all pass hand cannot have a suit, so turn off all pass

		// Deselect the previous suit
		if (this.currentHand.suit != null) {
			this.currentHand.suit.element.className = '';
		}

		this.currentHand.suit = newSuit;
		newSuit.element.className = 'selected'; // Select the button for this suit
	},

	// Increment or decrement the number of tricks won
	changeResult : function(change) {

		// If the hand was previously marked as won exactly
		if (this.currentHand.result == null) {

			// Hands where a result is not relevant
			if (this.currentHand.level == null || this.currentHand.allPass) {
				return;
			}

			// The result cannot be incremented
			if (this.currentHand.level == 7 && change > 0) {
				return;
			}

			// Set the number of tricks won to an integer if won exactly
			this.currentHand.result = this.currentHand.level + 6;
		}

		this.currentHand.result += change;

		// Minimum bound for result
		if (this.currentHand.result < 0) {
			this.currentHand.result = 0;
		}

		// Maximum bound for result
		if (this.currentHand.result > 13) {
			this.currentHand.result = 13;
		}

		this.updateResult();

	},

	// Set the level bid for this hand and update button selections accordingly
	setLevel : function(newLevel) {

		this.allPassOff(); // An all pass hand cannot have a level, so turn off all pass

		// If a level was previously selected
		if (this.currentHand.level != null) {
			elements.level[this.currentHand.level].className = ''; // Deselect the previous level

			// Update the result if neccessary
			if (this.currentHand.result != null) {
				this.changeResult(newLevel - this.currentHand.level);
			}

		}

		this.currentHand.level = newLevel;
		this.updateResult();
		elements.level[newLevel].className = 'selected'; // Select the button for this level
	}

};
