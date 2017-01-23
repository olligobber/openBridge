var selectedLevel = null;
var selectedSuit = null;
var selectedHand = null;

function New() {
	document.getElementById('buttons').style.display = null;
}

function Submit() {
	document.getElementById('buttons').style.display = 'none';
	if (selectedLevel != null) {
		selectedLevel.className = null;
	}
	if (selectedSuit != null) {
		selectedSuit.className = null;
	}
	if (selectedHand != null) {
		selectedHand.className = null;
	}
	selectedLevel = null;
	selectedSuit = null;
	selectedHand = null;
	return true;
}

function Edit(hand, button) {
	if (selectedHand != null) {
		if (!Submit()) {
			return false;
		}
	}
	selectedHand = button.parentElement.parentElement;
	selectedHand.className = 'selected';
	document.getElementById('buttons').style.display = null;
}

function SetLevel(level, button) {
	if (selectedLevel != null) {
		selectedLevel.className = null;
	}
	button.className = 'selected';
	selectedLevel = button;
}

function SetSuit(suit, button) {
	if (selectedSuit != null) {
		selectedSuit.className = null;
	}
	button.className = 'selected';
	selectedSuit = button;
}
