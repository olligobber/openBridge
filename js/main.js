Clubs.symbol = '&clubs;';
Clubs.index = 0;
Diamonds.symbol = '&diams;';
Diamonds.index = 1;
Hearts.symbol = '&hearts;';
Hearts.index = 2;
Spades.symbol = '&spades;';
Spades.index = 3;
NoTrumps.symbol = '\\';
NoTrumps.index = 4;

declarerIndex = ['N', 'E', 'S', 'W', 'AP'];

function renderResult(level, result) {
	if (result == null) {
		return "+0";
	}
	if (result > level + 5) {
		return "+" + (this.result - 6 - this.level);
	}
	return "" + (this.result - 6 - this.level);
}

Hand.prototype.render = function() {
	if (this.declarer == "AP") {
		return "AP";
	}
	if (this.declarer != null && this.suit != null && this.level != null) {
		var out = this.declarer + this.level + this.suit.symbol;
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

var selectedDeclarer = null;
var declarer = null;
var selectedLevel = null;
var level = null;
var selectedSuit = null;
var suit = null;
var selectedHand = null;
var result = null;
var allHands = [];
var we_games = 0;
var they_games = 0;

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

function setResult() {
	if (declarer == "AP" || level == null) {
		document.getElementById('result').innerHTML = "";
	}
	else if (result == null) {
		if (level < 4) {
			document.getElementById('result').innerHTML = "&nbsp;" + (level+6) + " " + renderResult(level, result);
		}
		else {
			document.getElementById('result').innerHTML = (level+6) + " " + renderResult(level, result);
		}
	}
	else {
		if (result < 10) {
			document.getElementById('result').innerHTML = "&nbsp;" + result + " " + renderResult(level, result);
		}
		else {
			document.getElementById('result').innerHTML = result + " " + renderResult(level, result);
		}
	}
}

function New() {
	Revert();
	document.getElementById('buttons').style.display = null;
	allHands.push(new Hand());
	selectedHand = allHands[allHands.length-1];
	selectedHand.index = allHands.length-1;
	if (allHands.length == 1) {
		document.getElementById('history').children[0].children[0].appendChild(elt("tr", elt("td"), elt("td", elt("button", "Edit")), elt("td", elt("button", "Delete"))));
	}
	else {
		document.getElementById('history').children[0].children[0].insertBefore(elt("tr", elt("td"), elt("td", elt("button", "Edit")), elt("td", elt("button", "Delete"))), document.getElementById('history').children[0].children[0].children[1]);
	}
	selectedHand.element = document.getElementById('history').children[0].children[0].children[1];
	selectedHand.element.className = 'selected';
	selectedHand.element.getElementsByTagName('button')[0].setAttribute('type', 'button');
	selectedHand.element.getElementsByTagName('button')[0].setAttribute('onClick', 'Edit(' + selectedHand.index + ');');
	selectedHand.element.getElementsByTagName('button')[1].setAttribute('type', 'button');
	selectedHand.element.getElementsByTagName('button')[1].setAttribute('onClick', 'Delete(' + selectedHand.index + ');');
}

function Delete(hand) {
	allHands[hand].element.outerHTML = "";
	allHands[hand] = null;
	renderScores();
}

function Revert() {
	document.getElementById('buttons').style.display = 'none';
	if (selectedHand != null && selectedHand.declarer != "AP" && !selectedHand.score()) {
		Delete(selectedHand.index);
	}
	if (selectedDeclarer != null) {
		selectedDeclarer.className = "";
	}
	if (selectedLevel != null) {
		selectedLevel.className = "";
	}
	if (selectedSuit != null) {
		selectedSuit.className = "";
	}
	if (selectedHand != null) {
		selectedHand.element.className = "";
	}
	selectedDeclarer = null;
	declarer = null;
	selectedLevel = null;
	level = null;
	selectedSuit = null;
	suit = null;
	selectedHand = null;
	document.getElementById('double').value = 1;
	document.getElementById('honours').value = 0;
	result = null;
	setResult();
}

function Submit() {
	if (declarer == "AP") {
		level = null;
		suit = null;
		document.getElementById('honours').value = null;
		document.getElementById('double').value = 1;
	}
	if (declarer == "AP" || (level != null && declarer != null && suit != null)) {
		selectedHand.declarer = declarer;
		selectedHand.level = level;
		selectedHand.suit = suit;
		selectedHand.double = parseInt(document.getElementById('double').value);
		if (declarer == "N" || declarer == "S") {
			selectedHand.honours = parseInt(document.getElementById('honours').value);
		}
		else {
			selectedHand.honours = - parseInt(document.getElementById('honours').value);
		}
		selectedHand.result = result;
		selectedHand.element.children[0].innerHTML = selectedHand.render();
		Revert();
		renderScores();
	}
}

function Edit(index) {
	Revert();
	selectedHand = allHands[index];
	selectedHand.element.className = 'selected';
	declarer = selectedHand.declarer;
	document.getElementById('declarer').children[declarerIndex.indexOf(declarer)].children[0].className = 'selected';
	level = selectedHand.level;
	if (level != null) {
		document.getElementById('level').children[level-1].children[0].className = 'selected';
	}
	suit = selectedHand.suit;
	if (suit != null) {
		document.getElementById('suit').children[suit.index].children[0].className = 'selected';
	}
	result = selectedHand.result;
	setResult();
	document.getElementById('double').value = selectedHand.double;
	if (declarer == "N" || declarer == "S") {
		 document.getElementById('honours').value = selectedHand.honours;
	}
	else {
		document.getElementById('honours').value = - selectedHand.honours;
	}
	document.getElementById('buttons').style.display = "";
}

function SetDeclarer(newDeclarer, button) {
	if (selectedDeclarer != null) {
		selectedDeclarer.className = "";
	}
	button.className = 'selected';
	selectedDeclarer = button;
	declarer = newDeclarer;
}

function SetLevel(newLevel, button) {
	if (selectedLevel != null) {
		selectedLevel.className = "";
		if (result != null) {
			ChangeResult(newLevel - level);
		}
	}
	button.className = 'selected';
	selectedLevel = button;
	level = newLevel;
	setResult();
}

function SetSuit(newSuit, button) {
	if (selectedSuit != null) {
		selectedSuit.className = "";
	}
	button.className = 'selected';
	selectedSuit = button;
	suit = newSuit;
}

function ChangeResult(change) {
	if (result == null) {
		if (level == 7 && change > 0) {
			return;
		}
		result = level + 6;
	}
	result += change;
	if (result < 0) {
		result = 0;
	}
	if (result > 13) {
		result = 13;
	}
	setResult();
}

function addAbove(who, what) {
	var element = document.getElementById(who + "_above");
	if (element.innerHTML == "") {
		element.innerHTML = what + element.innerHTML;
	}
	else {
		element.innerHTML = what + "<br>" + element.innerHTML;
	}
}

function renderScores() {
	// remove all scores
	document.getElementById("we_above").innerHTML = "";
	document.getElementById("they_above").innerHTML = "";
	for (var i = document.getElementById("above").parentElement.children.length-1; i >= 0; --i) {
		if (document.getElementById("above").parentElement.children[i].id == "") {
			document.getElementById("above").parentElement.children[i].outerHTML = "";
		}
	}

	var we_total = 0;
	var they_total = 0;
	var we_below = 0;
	var they_below = 0;
	we_games = 0;
	they_games = 0;

	for (var i = 0; i < allHands.length; ++i) {
		if (allHands[i] == null) {

		}
		else if (allHands[i].declarer != "AP") {
			if (allHands[i].declarer == "N" || allHands[i].declarer == "S") {
				allHands[i].vulnerable = (we_games == 1);
				allHands[i].score();
				if (allHands[i].scoreBelow != 0) {
					document.getElementById("botpad").parentElement.insertBefore(elt("tr", elt("td", "" + allHands[i].scoreBelow), elt("td")), document.getElementById("botpad"));
					we_below += allHands[i].scoreBelow;
					if (we_below > 99) {
						document.getElementById("botpad").previousSibling.className = "game";
						we_games++;
						we_below = 0;
						they_below = 0;
					}
				}
				if (allHands[i].scoreAbove != 0) {
					addAbove("we", allHands[i].scoreAbove);
				}
				if (allHands[i].scoreOpponent != 0) {
					addAbove("they", allHands[i].scoreOpponent);
				}
				if (we_games == 2) {
					if (they_games == 1) {
						addAbove("we", 500);
						we_total += 500;
					}
					else {
						addAbove("we", 700);
						we_total += 700;
					}
					we_games = 0;
					they_games = 0;
				}
				we_total += allHands[i].scoreBelow + allHands[i].scoreAbove;
				they_total += allHands[i].scoreOpponent;
			}
			else {
				allHands[i].vulnerable = (they_games == 1);
				allHands[i].score();
				if (allHands[i].scoreBelow != 0) {
					document.getElementById("botpad").parentElement.insertBefore(elt("tr", elt("td"), elt("td", "" + allHands[i].scoreBelow)), document.getElementById("botpad"));
					they_below += allHands[i].scoreBelow;
					if (they_below > 99) {
						document.getElementById("botpad").previousSibling.className = "game";
						they_games++;
						we_below = 0;
						they_below = 0;
					}
				}
				if (allHands[i].scoreAbove != 0) {
					addAbove("they", allHands[i].scoreAbove);
				}
				if (allHands[i].scoreOpponent != 0) {
					addAbove("we", allHands[i].scoreOpponent);
				}
				if (they_games == 2) {
					if (we_games == 1) {
						addAbove("they", 500);
						they_total += 500;
					}
					else {
						addAbove("they", 700);
						they_total += 700;
					}
					we_games = 0;
					they_games = 0;
				}

				they_total += allHands[i].scoreBelow + allHands[i].scoreAbove;
				we_total += allHands[i].scoreOpponent;
			}
		}
	}
	document.getElementById("total").children[0].innerHTML = we_total;
	document.getElementById("total").children[1].innerHTML = they_total;
}
