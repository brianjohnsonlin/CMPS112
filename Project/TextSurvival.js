// Yunyi Ding yding13@ucsc.edu
// Brian Lin bjlin@ucsc.edu

var MAP_WIDTH  = 55;
var MAP_HEIGHT = 25;
var RESOURCE_SCARCITY = 2;
var VIEW_DISTANCE = 3;
var Turns = 0;
var Environment = [];
var GameOver = false;
var Player = new function(){
    this.loc = [Math.floor(MAP_WIDTH/2),Math.floor(MAP_HEIGHT/2)];
    this.hunger = 100;
    this.health = 100;
    this.inv = [];
};

function init(){
	var start = Math.floor(Math.random()*5);
	if(start === 0) pushOutput("You wake up to the chirping of birds.");
	else if(start === 1) pushOutput("You wake up to the wind rustling the leaves.");
	else if(start === 2) pushOutput("You wake up to the hot sun on your face.");
	else if(start === 3) pushOutput("You wake up to the fresh smell of flowers.");
	else pushOutput("You wake up to the sudden fear of being alone.");
	pushOutput("You realize you're in the middle of the wilderness.");
	//generate environment
    for(var y = 0; y < MAP_HEIGHT; y++){
        Environment[y] = [];
        for(var x = 0; x < MAP_WIDTH; x++){
            var z = Math.floor((Math.random() * 100));
            if(z < RESOURCE_SCARCITY && !isPlayerOn(x,y)) Environment[y][x] = new rocks();
            else if(z >= RESOURCE_SCARCITY && z < RESOURCE_SCARCITY*2 && !isPlayerOn(x,y)) Environment[y][x] = new tree();
			else if(z >= RESOURCE_SCARCITY*2 && z < RESOURCE_SCARCITY*3 && !isPlayerOn(x,y)) Environment[y][x] = new pigs();
            else Environment[y][x] = new grass();
        }
    }
	see();
	pushOutput(printMap());
    //give player tool
    Player.inv.push(new item("tool", function(){
        if(Environment[Player.loc[1]][Player.loc[0]].type == 't'){
            Environment[Player.loc[1]][Player.loc[0]] = new grass();
			advance();
			$("#in").val("");
			if(Math.floor(Math.random()*100) < 25) pushOutput(addItem(new berry(), 1));
            return addItem(new wood(), 1);
        }else if(Environment[Player.loc[1]][Player.loc[0]].type == 'r'){
            Environment[Player.loc[1]][Player.loc[0]] = new grass();
            advance();
			return addItem(new stone(), 1);
        }else return "nothing happened";
    }));
    pushOutput("Type \"help\" to view available commands.");
}

function advance(){
	Turns++;
	Player.hunger--;
	see();
	//test for random event
}

//INVENTORY-------------------------------------------------------------
var item = function(n,f){ // item superclass
    this.name = n;
    this.quantity = 1;
    this.funct = f;
}

//various items
var stone = function(){return new item("stone", function(){return "nothing happened";})};
var wood = function(){return new item("wood", function(){return "nothing happened";})};
var berry = function(){return new item("berry", function(){
	if(Player.hunger < 100) Player.hunger += 5;
	this.quantity -= 1;
	return "you ate a berry";
})};
var meat = function(){return new item("meat", function(){
	if(Player.hunger < 100) Player.hunger += 25;
	this.quantity -= 1;
	return "you ate some meat";
})};

function addItem(it,quan){
    if(itemPlace(it.name) === -1){
        Player.inv.push(it);
        Player.inv[Player.inv.length-1].quantity = quan;
    }else Player.inv[itemPlace(it.name)].quantity += quan;
    return "received " + quan + " " + it.name;
}

function itemPlace(name){ //returns the position in inv array, -1 if nonexistent
    for(var i = 0; i < Player.inv.length; i++)
        if(Player.inv[i].name === name) return i;
    return -1;
}

//LOCATION and MOVEMENT-------------------------------------------------
function isPlayerOn(x,y){return Player.loc[0] == x && Player.loc[1] == y;}

var cell = function(t,v,e){
	this.type = t;
	this.visibility = v;
	this.encounter = e; //can be null
}

var rocks = function(){return new cell('r', false, null)};
var tree = function(){return new cell('t', false, null)};
var pigs = function(){return new cell('p', false, null)};
var grass = function(){
	var s;
	if(Math.floor((Math.random() * 100)) < 50) s = ',';
	else s = '.';	
	return new cell(s, false, null);
};

function see(){
	for(var x = -1 * VIEW_DISTANCE; x < VIEW_DISTANCE + 1; x++)
		for(var y = -1 * VIEW_DISTANCE; y < VIEW_DISTANCE + 1; y++)
			if(Player.loc[1] + y > -1
			&& Player.loc[1] + y < MAP_HEIGHT
			&& Player.loc[0] + x > -1
			&& Player.loc[0] + x < MAP_WIDTH
			&& !(Math.abs(x) == VIEW_DISTANCE && Math.abs(y) == VIEW_DISTANCE))
				Environment[Player.loc[1] + y][Player.loc[0] + x].visibility = true;
}

function seeAll(){ //just a cheat
	for(var y = 0; y < MAP_HEIGHT; y++)
        for(var x = 0; x < MAP_WIDTH; x++)
			Environment[y][x].visibility = true;
	pushOutput(printMap());
	return printMap();
}

function printMap(){
    var str = "";
    for(var x = 0; x < MAP_WIDTH + 2;  x++) str += '-';
    str += '\n';
    for(var y = 0; y < MAP_HEIGHT; y++){
        str += " |";
        for(var x = 0; x < MAP_WIDTH; x++){
            if(isPlayerOn(x,y)) str += "@";
            else if(Environment[y][x].visibility) str += Environment[y][x].type;
			else str += "&nbsp";
        }
        str += "|\n";
    }
    str += ' ';
    for(var x = 0; x < MAP_WIDTH + 2;  x++) str += '-';
    return str;
}

function move(d){
    if(d === 'n') if(Player.loc[1] > 0){
        Player.loc[1]--;
        advance();
    }
    if (d === 's') if(Player.loc[1] < MAP_HEIGHT - 1){
        Player.loc[1]++;
        advance();
    }
    if (d === 'w') if(Player.loc[0] > 0){
        Player.loc[0]--;
        advance();
    }
    if (d === 'e') if(Player.loc[0] < MAP_WIDTH - 1){
        Player.loc[0]++;
        advance();
    }
}

//Input Output Stuff----------------------------------------------------
var PreviousCommands = [];
var CurrentCommand = "";
var CommandPosition = 0;

function pushOutput(s){
	if(s === undefined || s === null) return;
	while(s.indexOf("<") > -1) s = s.replace("<", "&lt");
	while(s.indexOf(">") > -1) s = s.replace(">", "&gt");
	while(s.indexOf("\n") > -1) s = s.replace("\n", "<br>");
	document.getElementById("out").innerHTML = $('#out').html() + s + "<br>";
	document.getElementById("out").scrollTop = document.getElementById("out").scrollHeight;
	if(Player.hunger < 1 || Player.health < 1 && !GameOver){
        GameOver = true;
        document.getElementById("out").innerHTML = $('#out').html() + "You died!!!\nGame Over" + "<br>";
        document.getElementById("out").scrollTop = document.getElementById("out").scrollHeight;
    }
}

$("#enter").click(function(){
	if($("#in").val() !== "" && !GameOver){
		PreviousCommands.push($("#in").val());
		CommandPosition = PreviousCommands.length;
		parseInput($("#in").val());
		$("#in").val("");
	}
});

$("#in").keyup(function(event){
    if(event.keyCode == 13){
		if($("#in").val() !== "" && !GameOver){
			pushOutput("\n> " + $("#in").val());
			PreviousCommands.push($("#in").val());
			CommandPosition = PreviousCommands.length;
			parseInput($("#in").val());
			$("#in").val("");
		}
    }else if(event.keyCode == 38 && CommandPosition > 0){
		CommandPosition--;
		if(CommandPosition === PreviousCommands.length - 1) CurrentCommand = $("#in").val();
		$("#in").val(PreviousCommands[CommandPosition]);
	}else if(event.keyCode == 40 && CommandPosition < PreviousCommands.length){
		CommandPosition++;
		if(CommandPosition === PreviousCommands.length) $("#in").val(CurrentCommand);
		else $("#in").val(PreviousCommands[CommandPosition]);
	}
});

function parseInput(str){
    if(!GameOver){
        var args = str.toLowerCase().split(" ");
        switch(args[0]) {
            case "inv": pushOutput(inv()); break;
            case "help": pushOutput(help()); break;
            case "status": pushOutput(status()); break;
            case "observe": pushOutput(observe()); break;
            case "north": pushOutput(n()); break;
            case "west": pushOutput(w()); break;
            case "east": pushOutput(e()); break;
            case "south": pushOutput(s()); break;
            case "w": pushOutput(n()); break;
            case "a": pushOutput(w()); break;
            case "d": pushOutput(e()); break;
            case "s": pushOutput(s()); break;
            case "clear" : clear(); break;
            case "use":
                if(args.length !== 2) pushOutput("Usage: use <item name>");
                else pushOutput(use(args[1]));
                break;
            default: pushOutput(args[0] + ": command not found.\nType \"help\" to view available commands.");
        }
    }
}

init(); //run init at beginning
//USER FUNCTIONS--------------------------------------------------------
function help(){
    return "Commands:\n"
		  +"|help - display this\n"
          +"|inv - displays your inventory\n"
          +"|use <item name> - use an item\n"
          +"|status - shows your status\n"
		  +"|observe - observe your surroundings\n"
          +"|w / North - move north\n"
          +"|a / West - move west\n"
          +"|d / East - move east\n"
          +"|s / South - move south\n";
}

function use(name){
    if(itemPlace(name) === -1) return "you don't have that item";
    else{
		var result = Player.inv[itemPlace(name)].funct();
		for(var i = 0; i < Player.inv.length; i++)
			if(Player.inv[i].quantity < 1) Player.inv.splice(i,1);
		return result;
	}
}

function n(){move('n');return printMap();}
function s(){move('s');return printMap();}
function w(){move('w');return printMap();}
function e(){move('e');return printMap();}

function inv(){
    var s = "Inventory:\n";
    for(var i = 0; i < Player.inv.length; i++)
        s += "| " + Player.inv[i].quantity + " " + Player.inv[i].name + "\n";
    return s;
}

function status(){
    return "Health: " + Player.health + "\nHunger: " + Player.hunger;
}

function observe(){
	switch(Environment[Player.loc[1]][Player.loc[0]].type){
		case 't': return "it's a tree"; break;
		case 'r': return "it's a lot of rocks"; break;
		case 'p': return "it's a herd of pigs"; break;
		default: return "there's not much around";
	}
}

function clear(){document.getElementById("out").innerHTML = ""}
