var main_js_version = "1.0.1"

if (typeof hand_js_version == "undefined") {
	console.error("main.js detected that hand.js was not loaded");
}

if (hand_js_version != "1.1") {
	console.error("main.js detected that the current version of hand.js was not correctly loaded");
}

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

var players = {

	North : {
		element : document.getElementById('declarer').children[0].children[0],
		symbol : 'N',
		team : 'we',
		opponent : 'they'
	},

	East : {
		element : document.getElementById('declarer').children[1].children[0],
		symbol : 'E',
		team : 'they',
		opponent : 'we'
	},

	South : {
		element : document.getElementById('declarer').children[2].children[0],
		symbol : 'S',
		team : 'we',
		opponent : 'they'
	},

	West : {
		element : document.getElementById('declarer').children[3].children[0],
		symbol : 'W',
		team : 'they',
		opponent : 'we'
	}

};

var elements = {

	allPass : document.getElementById('declarer').children[4].children[0],

	level : [null],

	result : document.getElementById('result'),

	buttons : document.getElementById('buttons'),

	double : document.getElementById('double'),

	honours : document.getElementById('honours'),

	history : document.getElementById('history').children[0].children[0],

	above : {
		we : document.getElementById("we_above"),
		they : document.getElementById("they_above")
	},

	scores : document.getElementById("scorepad").children[0].children[1],

	botpad : document.getElementById("botpad"),

	total : {
		we : document.getElementById("total").children[0],
		they : document.getElementById("total").children[1]
	}

};

for (var i=0; i<7; ++i) {
	elements.level.push(document.getElementById('level').children[i].children[0]);
}

function renderResult(level, result) {
	if (result == null) {
		return "+0";
	}
	if (result > level + 5) {
		return "+" + (result - 6 - level);
	}
	return "" + (result - 6 - level);
}

function elt(type) { // Borrowed from http://eloquentjavascript.net/13_dom.html
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

Hand.prototype.render = function() {
	if (this.allPass) {
		return "AP";
	}
	if (this.declarer != null && this.score()) {
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

Hand.prototype.index = null;
Hand.prototype.element = null;
Hand.prototype.declarer = null;
Hand.prototype.allPass = false;

var scorepad = {
	currentHand : null,
	allHands : [],
	handCount : 0,
	scoreElements : [],

	clearHand : function() {
		elements.buttons.style.display = 'none';

		if (this.currentHand == null) {
			return;
		}

		if (this.currentHand.index == null) {
			this.currentHand.element.outerHTML = "";
		}
		else {
			this.currentHand.element.className = "";
		}

		if (this.currentHand.allPass) {
			elements.allPass.className = "";
		}
		else {
			if (this.currentHand.level != null) {
				elements.level[this.currentHand.level].className = "";
			}
			if (this.currentHand.suit != null) {
				this.currentHand.suit.element.className = "";
			}
			if (this.currentHand.declarer != null) {
				this.currentHand.declarer.element.className = "";
			}
		}

		elements.double.value = 1;
		elements.honours.value = 0;
		elements.result.innerHTML = "";
		this.currentHand = null;
	},

	updateResult : function() {
		if (this.currentHand == null) {
			elements.result.innerHTML = "";
		}
		else if (this.currentHand.declarer == "AP" || this.currentHand.level == null) {
			elements.result.innerHTML = "";
		}
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

	updateScores : function() {
		elements.above.we.innerHTML = "";
		elements.above.they.innerHTML = "";
		while (this.scoreElements.length > 0) {
			this.scoreElements.pop().outerHTML = "";
		}

		var totals = {
			we : 0,
			they : 0
		};

		var below = {
			we : 0,
			they : 0
		};

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
			this.allHands[i].vulnerable = (games[this.allHands[i].declarer.team] == 1);
			this.allHands[i].score();

			if (this.allHands[i].scoreBelow != 0) {
				totals[this.allHands[i].declarer.team] += this.allHands[i].scoreBelow;
				if (this.allHands[i].declarer.team == "we") {
					elements.scores.insertBefore(elt("tr", elt("td", "" + this.allHands[i].scoreBelow), elt("td")), elements.botpad);
				}
				else {
					elements.scores.insertBefore(elt("tr", elt("td"), elt("td", "" + this.allHands[i].scoreBelow)), elements.botpad);
				}
				this.scoreElements.push(elements.botpad.previousSibling);
				below[this.allHands[i].declarer.team] += this.allHands[i].scoreBelow;
				if (below[this.allHands[i].declarer.team] > 99) {
					elements.botpad.previousSibling.className = "game";
					games[this.allHands[i].declarer.team]++;
					below.we = 0;
					below.they = 0;
				}
			}

			if (this.allHands[i].scoreAbove != 0) {
				totals[this.allHands[i].declarer.team] += this.allHands[i].scoreAbove;
				elements.above[this.allHands[i].declarer.team].innerHTML = this.allHands[i].scoreAbove + "<br>" + elements.above[this.allHands[i].declarer.team].innerHTML;
			}

			if (this.allHands[i].scoreOpponent != 0) {
				totals[this.allHands[i].declarer.opponent] += this.allHands[i].scoreOpponent;
				elements.above[this.allHands[i].declarer.opponent].innerHTML = this.allHands[i].scoreOpponent + "<br>" + elements.above[this.allHands[i].declarer.opponent].innerHTML;
			}

			if (games[this.allHands[i].declarer.team] == 2) {
				if (games[this.allHands[i].declarer.opponent] == 1) {
					totals[this.allHands[i].declarer.team] += 500;
					elements.above[this.allHands[i].declarer.team].innerHTML = "500<br>" + elements.above[this.allHands[i].declarer.team].innerHTML;
				}
				else {
					totals[this.allHands[i].declarer.team] += 700;
					elements.above[this.allHands[i].declarer.team].innerHTML = "700<br>" + elements.above[this.allHands[i].declarer.team].innerHTML;
				}
				games.we = 0;
				games.they = 0;
			}

		}

		elements.total.we.innerHTML = totals.we;
		elements.total.they.innerHTML = totals.they;

	},

	newHand : function() {
		this.clearHand();
		this.currentHand = new Hand();
		elements.history.insertBefore(elt("tr", elt("td"), elt("td", elt("button", "Edit")), elt("td", elt("button", "Delete"))), elements.history.children[1]);
		this.currentHand.element = elements.history.children[1];
		this.currentHand.element.className = 'selected';
		this.currentHand.element.getElementsByTagName('button')[0].setAttribute('type', 'button');
		this.currentHand.element.getElementsByTagName('button')[1].setAttribute('type', 'button');
		elements.buttons.style.display = null;
	},

	removeHand : function(index) {
		this.allHands[index].element.outerHTML = "";
		this.allHands[index] = null;
		this.updateScores();
		this.handCount--;
		if (this.handCount == 0) {
			this.allHands = [];
		}
	},

	submitHand : function() {
		if (this.currentHand.allPass || this.currentHand.score()) {
			this.currentHand.double = parseInt(elements.double.value);
			this.currentHand.honours = parseInt(elements.honours.value);
			if (this.currentHand.declarer.team == "they") {
				this.currentHand.honours = - this.currentHand.honours;
			}
			this.currentHand.element.children[0].innerHTML = this.currentHand.render();

			if (this.currentHand.index == null) {
				this.currentHand.index = this.allHands.length;
				this.allHands.push(this.currentHand);
				this.currentHand.element.getElementsByTagName('button')[0].setAttribute('onClick', 'scorepad.editHand(' + this.currentHand.index + ');');
				this.currentHand.element.getElementsByTagName('button')[1].setAttribute('onClick', 'scorepad.removeHand(' + this.currentHand.index + ');');
			}
			else {
				this.allHands[this.currentHand.index] = this.currentHand;
			}

			this.clearHand();
			this.updateScores();
		}

	},

	editHand : function(index) {
		this.clearHand();
		this.currentHand = Object.create(this.allHands[index]);
		this.currentHand.element.className = 'selected';

		if (this.currentHand.allPass) {
			elements.allPass.className = 'selected';
		}
		else {
			this.currentHand.declarer.element.className = 'selected';
			elements.level[this.currentHand.level].className = 'selected';
			this.currentHand.suit.element.className = 'selected';
		}

		elements.double.value = this.currentHand.double;
		if (this.currentHand.declarer.team == 'we') {
			elements.honours.value = this.currentHand.honours;
		}
		else {
			elements.honours.value = - this.currentHand.honours;
		}

		this.updateResult();

		elements.buttons.style.display = null;
	},

	allPassOn : function() {
		if (!this.currentHand.allPass) {
			this.currentHand.allPass = true;
			elements.allPass.className = 'selected';
			if (this.currentHand.declarer != null) {
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

	allPassOff : function() {
		if (this.currentHand.allPass) {
			this.currentHand.allPass = false;
			elements.allPass.className = '';
			if (this.currentHand.declarer != null) {
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

	setDeclarer : function(newDeclarer) {
		this.allPassOff();
		if (this.currentHand.declarer != null) {
			this.currentHand.declarer.element.className = '';
		}
		this.currentHand.declarer = newDeclarer;
		newDeclarer.element.className = 'selected';
	},

	setSuit : function(newSuit) {
		this.allPassOff();
		if (this.currentHand.suit != null) {
			this.currentHand.suit.element.className = '';
		}
		this.currentHand.suit = newSuit;
		newSuit.element.className = 'selected';
	},

	changeResult : function(change) {
		if (this.currentHand.result == null) {
			if (this.currentHand.level == 7 && change > 0) {
				return;
			}
			this.currentHand.result = this.currentHand.level + 6;
		}

		this.currentHand.result += change;

		if (this.currentHand.result < 0) {
			this.currentHand.result = 0;
		}

		if (this.currentHand.result > 13) {
			this.currentHand.result = 13;
		}

		this.updateResult();

	},

	setLevel : function(newLevel) {
		this.allPassOff();
		if (this.currentHand.level != null) {
			elements.level[this.currentHand.level].className = '';
			if (this.currentHand.result != null) {
				this.changeResult(newLevel - this.currentHand.level);
			}
		}
		this.currentHand.level = newLevel;
		this.updateResult();
		elements.level[newLevel].className = 'selected';
	}

};
