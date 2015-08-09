"use strict";

// colors
const MAP_COLOR            = '#ffffe8';
const MAP_BACKGROUND_COLOR = '#ffffff';
const FILLED_COLOR         = '#f0f040';
const UNIT_COLOR           = '#ffb000';
const UNIT_OVERLAP_COLOR   = '#ff6000';
const PIVOT_COLOR          = '#209010';

// directions and moves
const DIR_E   = 0;
const DIR_SE  = 1;
const DIR_SW  = 2;
const DIR_W   = 3;
const DIR_NW  = 4;
const DIR_NE  = 5;
const ROT_CW  = 6;
const ROT_CCW = 7;
const LOCK    = 8;
const STOP    = 9;

// quality
const Q_DEPTH_FACTOR = 100;
const Q_COVERED = 30;
const Q_PART_COVERED = 10;


var size = 42;

var problem = {};

var unit;
var prevPositions = [];
var numSpawned = 0;
var score = 0;


// ===== Sounds =====

var sound_end, sound_lock, sound_move, sound_row;
var sounds = false;

function playSound(sound) {
    if (sounds) {
        sound.play();
    }
}


// ===== RNG =====

var seed = 0;

function lopart(n) {
    return n & 65535;
}

function hipart(n) {
    return n >>> 16;
}

function mul32_32(a, b) {
    var r0 = (lopart(a) * lopart(b));
    var r1 = (hipart(a) * lopart(b)) << 16;
    var r2 = (lopart(a) * hipart(b)) << 16;
    return (r0 + r1 + r2) >>> 0;
}

function nextRandom() {
    var n = (seed >>> 16) & ((1 << 15) - 1);
    seed = (mul32_32(1103515245, seed) + 12345) >>> 0;
    return n;
}

// =====

function isOnBoard(c) {
    return (c.x >= 0 && c.x < problem.width &&
            c.y >= 0 && c.y < problem.height);
}


function isFilled(c) {
    var f = problem.filled;
    var n = f.length;
    for (var i = 0; i < n; i++) {
        if (cellEquals(f[i], c)) {
            return true;
        }
    }
    return false;
}

function isFilledOrOff(c) {
    return isFilled(c) || !isOnBoard(c);
}

function cellEquals(a, b) {
    return a.x === b.x && a.y === b.y;
}

function cellsEquals(a, b) {
    var i;
    var k;
    var cell;
    var found;
    var n = a.length;
    if (n !== b.length) {
        return false;
    }

    for (i = 0; i < n; i++) {
        cell = a[i];
        found = false;
        for (k = 0; k < n && !found; k++) {
            if (cellEquals(cell, b[k])) {
                found = true;
            }
        }
        if (!found) {
            return false;
        }
    }

    return true;
}



function unitEquals(a, b) {
    if (!cellEquals(a.pivot, b.pivot)) {
        return false;
    }
    if (!cellsEquals(a.members, b.members)) {
        return false;
    }
    return true;
}


function isValidLocation(unit) {
    var i;
    var n = unit.members.length;
    for (i = 0; i < n; i++) {
        if (isFilledOrOff(unit.members[i])) {
            return false;
        }
    }

    for (i = prevPositions.length - 1; i >= 0; i--) {
        if (unitEquals(unit, prevPositions[i])) {
            return false;
        }
    }
    
    return true;
}

function atBottom(unit) {
    var dim = unitDimensions(unit);
    return dim.max_y >= (problem.height - 1);
}

function rowFull(row) {
    var numCells = 0;
    problem.filled.forEach(function (c) {
        if (c.y === row) {
            numCells++;
        }
    });
    return numCells === problem.width;
}

function clearRow(row) {
    var n = [];
    problem.filled.forEach(function (cell) {
        if (cell.y > row) {
            n.push(cell);
        }
        else if (cell.y < row) {
            n.push({x: cell.x, y: cell.y + 1});
        }
        // and don't push if === row
    });
    problem.filled = n;
}

function topmost_row_used() {
    var top = problem.height;
    problem.filled.forEach(function (cell) { top = Math.min(top, cell.y); });
    return top;
}

function cover_badness(c) {
    var nw = isFilledOrOff(moveCell(c, DIR_NW));
    var ne = isFilledOrOff(moveCell(c, DIR_NE));
    if (nw) {
        return ne ? Q_COVERED : Q_PART_COVERED;
    } else {
        return ne ? Q_PART_COVERED : 0;
    }
}

// evaluate quality of current board position
// lower is better
function quality() {
    var row, col, cell;
    var q = 0;
    // "low" is good
    q += 100 * (problem.height - topmost_row_used());
    // holes are bad
    for (row = 1; row < problem.height; row++) {
        for (col = 0; col < problem.width; col++) {
            cell = {x: col, y: row};
            if (!isFilled(cell)) {
                q += cover_badness(cell);
            }
        }
    }
    return q;
}



// unit dimensions
function unitDimensions(unit) {
    var min_x =  100000;
    var max_x = -100000;
    var min_y =  100000;
    var max_y = -100000;
    unit.members.forEach(function (cell) {
        if (cell.x < min_x) { min_x = cell.x; }
        if (cell.x > max_x) { max_x = cell.x; }
        if (cell.y < min_y) { min_y = cell.y; }
        if (cell.y > max_y) { max_y = cell.y; }
    });
    return { min_x: min_x, max_x: max_x,
             min_y: min_y, max_y: max_y };
}


var ls_old = 0;

function lockUnit() {
    playSound(sound_lock);
    unit.members.forEach(function (cell) {
        problem.filled.push(cell);
    });

    var ls = 0;
    var row;
    for (row = 0; row < problem.height; row++) {
        if (rowFull(row)) {
            ls++;
            clearRow(row);
            playSound(sound_row);
        }
    }
    var points = unit.members.length + 100 * (1 + ls) * ls / 2;
    var line_bonus = 0;
    if (ls_old > 1) {
        line_bonus = Math.floor((ls_old - 1) * points / 10);
    }
    score += points + line_bonus;
    ls_old = ls;
}


function loadProblem(f) {
    var reader = new FileReader();
    reader.onload = function(e) {
        problem = JSON.parse(e.target.result);
        seed = problem.sourceSeeds[0];
        numSpawned = 0;
        score = 0;
        ls_old = 0;
        nextUnit();
        $('#msg').text("Problem loaded: " + problem.id + ', ' + f.name);
        $('#lbl_units').text(problem.units.length);
        $('#unit_num').val(0);
        $('#unit_num').attr('max', problem.units.length - 1);
        $('#solution').text('');
        solution = [];
        prevPositions = [];
        drawMap();
        drawUnit();
    };
    reader.readAsText(f);
}

var extSol = [];
function loadSolution(f) {
    var reader = new FileReader();
    reader.onload = function (e) {
        extSol = JSON.parse(e.target.result);
    };
    reader.readAsText(f);
}


function showCoord(cell) {
    return cell.x.toString() + ',' + cell.y;
}

function parseCoord(s) {
    var col = s.charCodeAt(0) - 64;
    var row = parseInt(s.substring(1));
    return {x:col, y:row};
}


function drawHex(ctx) {
    var y;
    var h = size / 4;
    var t = size / 2;
    var md = t * Math.sqrt(3);
    ctx.beginPath();
    y = (size - md) / 2;
    ctx.moveTo(t, size);
    ctx.lineTo(size - y, 3*h);
    ctx.lineTo(size - y, h);
    ctx.lineTo(t, 0);
    ctx.lineTo(y, h);
    ctx.lineTo(y, 3*h);
    ctx.lineTo(t, size);
    ctx.fill();
    ctx.stroke();
    ctx.closePath();
}

function drawPivot(ctx) {
  ctx.save();
  ctx.translate(size / 2, size / 2);
  ctx.fillStyle = PIVOT_COLOR;
  ctx.beginPath();
  ctx.arc(0, -size / 7, size / 4.5, 0, Math.PI*2, true); 
  ctx.fill();
  ctx.closePath();
  ctx.restore();
}

function baseCoord(c) {
    var xoff = size * Math.sqrt(3) / 2;
    var yoff = 0.75 * size;
    var x = -5;
    if (c.y & 1) {
        x += xoff / 2;
    }
    return {x: x + c.x * xoff, y: c.y * yoff};
}

function mapToCell(x, y) {
    var xoff = size * Math.sqrt(3) / 2;
    var yoff = 0.75 * size;
    var row = parseInt((y + 3) / yoff);
    if ((row & 1) === 0) {
        x = x - (xoff / 2);
    }
    var col = parseInt((x+3) / xoff);
    return { x: col, y: row };
}


function drawMap() {
    var width = problem.width;
    var height = problem.height;
    var xoff = size * Math.sqrt(3) / 2;
    var yoff = 0.75 * size;
    var m = document.getElementById('map');
    m.width = xoff * width + xoff / 2;
    m.height = yoff * (height + 0.333);
    var c = m.getContext('2d');
    var row, col, w;
    var xy, cell;

    c.fillStyle = MAP_BACKGROUND_COLOR;
    c.fillRect(0, 0, m.width, m.height);
    c.fillStyle = MAP_COLOR;
    c.strokeStyle = 'black';
    c.save();
    for (row = 0; row < height; row++) {
        for (col = 0; col < width; col++) {
            cell = {x:col,y:row};
            if (isFilled(cell)) {
                c.fillStyle = FILLED_COLOR;
            } else {
                c.fillStyle = MAP_COLOR;
            }
            xy = baseCoord(cell);
            c.save();
            c.translate(xy.x,xy.y);
            drawHex(c);
            if (size >= 40) {
                c.strokeText(showCoord(cell), xoff * 0.5, size / 2);
            }
            c.restore();
        }
    }
    c.restore();
}

function drawUnit() {
    if (!unit) {
        return;
    }
    var m = document.getElementById('map');
    var c = m.getContext('2d');
    c.fillStyle = UNIT_COLOR;
    c.strokeStyle = 'black';
    c.save();
    unit.members.forEach(function (cell) {
        var xy = baseCoord(cell);
        c.save();
        if (isFilled(cell)) {
            c.fillStyle = UNIT_OVERLAP_COLOR;
        }
        c.translate(xy.x, xy.y);
        drawHex(c);
        c.restore();
    });

    if (isOnBoard(unit.pivot)) {
        var xy = baseCoord(unit.pivot);
        c.save();
        c.translate(xy.x, xy.y);
        drawPivot(c);
        c.restore();
    }
}

function spawnUnit(index) {
    unit = problem.units[index];
    var dim = unitDimensions(unit);
    var yoffs = 0 - dim.min_y;
    var width = 1 + dim.max_x - dim.min_x;
    var space = problem.width - width;
    var xoffs = Math.floor(space / 2) - dim.min_x;
    // alert('width: ' + width + ', offs ' + xoffs + ',' + yoffs);
    unit = translateUnit(unit, xoffs, yoffs);
}


function translateCell(cell, x, y) {
    if (y & 1) {
        // odd number of rows - have to correct for row "indentation"
        if (cell.y & 1) {
            ;
        } else {
            x -= 1;
        }
    }
    return { x: cell.x + x, y: cell.y + y };
}


//                E,     SE,     SW,      W,        NW,       NE
const OFFS =  [  [ 1, 0,  0, 1,   -1, 1,   -1, 0,    -1, -1,   0, -1 ],   // even row
                 [ 1, 0,  1, 1,   0, 1,    -1, 0,    0, -1,    1, -1 ] ]; // odd row

function moveCell(cell, direction) {
    var offs = OFFS[cell.y & 1];
    var d = 2 * direction;
    return { x: cell.x + offs[d], y: cell.y + offs[d+1] };
}



function moveUnit(unit, direction) {
    return {
             pivot: moveCell(unit.pivot, direction),
             members: unit.members.map(function (c) { return moveCell(c, direction); })
           };
}


function translateUnit(unit, x, y) {
    return {
             pivot: translateCell(unit.pivot, x, y),
             members: unit.members.map(function (c) { return translateCell(c, x, y); })
           };
}

function rotateCCW(unit) {
    var i;
    var u = unit;
    for (i = 0; i < 5; i++) {
        u = rotateCW(u);
    }
    return u;
}

function cubeToOffset(c) {
    var col = (c.x + (c.z - (c.z & 1)) / 2) | 0;
    var row = c.z;
    return { x: col, y: row };
}

function offsetToCube(c) {
    var x = (c.x - (c.y - (c.y & 1)) / 2) | 0;
    var z = c.y;
    var y = -x - z;
    return {x:x, y:y, z:z};
}

function computeRot(c, p) {
    var cc = offsetToCube(c);
    var cp = offsetToCube(p);
    var d = { x: cp.x - cc.x, y: cp.y - cc.y, z: cp.z - cc.z };
    var r = { x: -d.z, y: -d.x, z: -d.y };
    var p = { x: cc.x + r.x, y: cc.y + r.y, z: cc.z + r.z };
    return cubeToOffset(p);
}



function rotateCW(unit) {
    return {
        pivot: unit.pivot,
        members: unit.members.map(function (c) {
            return computeRot(unit.pivot, c);
        })
      };
}

function moveUnitX(unit, direction) {
    var u = moveUnit(unit, direction);
    if (isValidLocation(u)) {
        return u;
    }
    playSound(sound_end);
    return unit;
}

function nextUnit() {
    if (numSpawned >= problem.sourceLength) {
        unit = null;
        return;
    }
    numSpawned++;    
    var n = nextRandom();
    prevPositions = [];
    spawnUnit(n % problem.units.length);
}


var solution = [];

var WORDS = [["ei!", "243"],
             ["ia! ia!", "4435443"],
             ["r'lyeh", "135224"],
             ["yuggoth", "2k445k4"]
            ]; 

function chant(s, words) {
    words.forEach(function (w) {
        s = s.replace(new RegExp(w[1], 'g'), w[0]);
    });
    return s;
}

function encodeSolution(s) {
    var r = '';
    s.forEach(function (move) {
        var c = '0';
        switch (move) {
        case DIR_E: c = '2'; break;
        case DIR_W: c = '3'; break;
        case DIR_SW: c = '4'; break;
        case DIR_SE: c = '5'; break;
        case ROT_CW: c = '1'; break;
        case ROT_CCW: c = 'k'; break;
        case LOCK: c = '4'; break;
        }
        if (c !== '0') {
            r += c;
        }
    });
    return chant(r, WORDS);
}

function executeMove(move, newUnit) {
    prevPositions.push(unit);
    unit = newUnit;
    solution.push(move);
}

function tryMove(move) {
    return tryMoveX(move, false);
}

function tryUserMove(move) {
    return tryMoveX(move, true);
}


function tryMoveX(move, lockIfInvalid) {
    var u;
    if (move < ROT_CW) {
        u = moveUnit(unit, move);
        if (isValidLocation(u)) {
            executeMove(move, u);
            return true;
        } else if (lockIfInvalid) {
            solution.push(move);
            lockUnit();
            nextUnit();
            return true;
        }
        return false;
    }
    if (move === ROT_CW) {
        u = rotateCW(unit);
        if (isValidLocation(u) && !unitEquals(u, unit)) {
            executeMove(move, u);
            return true;
        } else if (lockIfInvalid) {
            solution.push(move);
            lockUnit();
            nextUnit();
            return true;
        }
        return false;
    }
    if (move === ROT_CCW) {
        u = rotateCCW(unit);
        if (isValidLocation(u) && !unitEquals(u, unit)) {
            executeMove(move, u);
            return true;
        } else if (lockIfInvalid) {
            solution.push(move);
            lockUnit();
            nextUnit();
            return true;
        }
        return false;
    }
    if (move === LOCK) {
        solution.push(move);
        lockUnit();
        nextUnit();
        return true;
    }
    if (move === STOP) {
        playSound(sound_end);
        unit = null;
        return true;
    }
    alert('invalid move: ' + move);
}


function computeMove() {
    if (!isValidLocation(unit)) {
        tryMove(STOP);
    }
    else if (atBottom(unit)) {
        tryMove(LOCK);
    }
    else {
        tryMove(DIR_SW) || tryMove(DIR_SE) || tryMove(DIR_W) || tryMove(DIR_E)
          || tryMove(ROT_CW) || tryMove(ROT_CCW)
          || tryMove(LOCK);
        // drawMap();
        // drawUnit();
        // $('#solution').text(encodeSolution(solution));
        // $('#msg').text('Score: ' + score + ', spawned: ' + numSpawned);
    }
}

function humanMove(move) {
    if (!isValidLocation(unit)) {
        tryMove(STOP);
    }
    else {
        tryUserMove(move);
        drawMap();
        drawUnit();
        $('#solution').text(encodeSolution(solution));
        $('#msg').text('Score: ' + score + ', spawned: ' + numSpawned);
    }
}


function runSolution() {
    var i = 0;
    var n = extSol[0].solution.length;
    for (i = 0; i < n && unit; i++) {
        var move;
        var c = extSol[0].solution.charAt(i);
        if ('dqrvz1'.indexOf(c) >= 0) {
            move = ROT_CW;
        }
        else if ('kstuwx'.indexOf(c) >= 0) {
            move = ROT_CCW;
        }
        else if ('p\'!.03'.indexOf(c) >= 0) {
            move = DIR_W;
        }
        else if ('bcefy2'.indexOf(c) >= 0) {
            move = DIR_E;
        }
        else if ('aghij4'.indexOf(c) >= 0) {
            move = DIR_SW;
        }
        else if ('lmno 5'.indexOf(c) >= 0) {
            move = DIR_SE;
        }
        else {
            alert('unsupported move: "' + c + '"');
        }
        if (!tryMove(move)) {
            tryMove(LOCK);
        }
    }
}



$(document).ready(function(){
    $('#problem').live('change', function (e) {
        loadProblem(e.target.files[0]);
    });

    $('#btn_SW').click(function() {
        humanMove(DIR_SW);
    });
    $('#btn_SE').click(function() {
        humanMove(DIR_SE);
    });
    $('#btn_E').click(function() {
        humanMove(DIR_E);
    });
    $('#btn_W').click(function() {
        humanMove(DIR_W);
    });
    $('#btn_CCW').click(function() {
        humanMove(ROT_CCW);
    });
    $('#btn_CW').click(function() {
        humanMove(ROT_CW);
    });

    $('#btn_move').click(function () {
        computeMove();
    });
    $('#btn_run').click(function () {
        while (unit != null && numSpawned <= problem.sourceLength) {
            computeMove();
        }
        drawMap();
        if (numSpawned <= problem.sourceLength && unit != null) {
            drawUnit();
        }
        $('#solution').text(encodeSolution(solution));
        $('#msg').text('Score: ' + score + ', spawned: ' + numSpawned);
    });

    $('#sol').live('change', function (e) {
        loadSolution(e.target.files[0]);
    });
    $('#btn_simulate').click(function () {
        runSolution();
        drawMap();
        drawUnit();
        $('#solution').text(encodeSolution(solution));
        $('#msg').text('Score: ' + score + ', spawned: ' + numSpawned);
    });
    
    sound_move = new Audio('move.wav');
    sound_lock = new Audio('lock.wav');
    sound_end = new Audio('end.wav');
    sound_row = new Audio('rowfull.wav');
});
