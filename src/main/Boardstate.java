package main;

import commands.Command;

import org.json.JSONArray;
import org.json.JSONObject;

import solver.VisitedState;
import units.Coordinate;
import units.Unit;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class Boardstate {

    private final int width;
    private final int height;

    // filled cells, row-wise
    private final boolean[] filled;
    
    private int cellIndex(int x, int y) {
        return y * width + x;
    }
    
    private int cellIndex(Coordinate c) {
        return cellIndex(c.x, c.y);
    }
    
    private int linesClearedOld = 0;
    private final JSONObject sourceJson;

    public Boardstate(JSONObject json) {
        this.height = json.getInt("height");
        this.width = json.getInt("width");
        this.filled = convertFilled(json.getJSONArray("filled"));
        this.sourceJson = json;
    }
    
    public Boardstate getInitialStateClone() {
    	return new Boardstate(sourceJson);
    }

    private boolean[] convertFilled(JSONArray filledJson) {
        boolean[] filled = new boolean[width*height];
        int n = filledJson.length();
        for (int i = 0; i < n; i++) {
            JSONObject point = filledJson.getJSONObject(i);
            int x = point.getInt("x");
            int y = point.getInt("y");
            filled[cellIndex(x,y)] = true;
        }
        return filled;
    }

    public boolean isFilled(int x, int y) {
        return filled[cellIndex(x,y)];
    }

    private boolean isFilled(Coordinate coordinate) {
        return isFilled(coordinate.x, coordinate.y);
    }
    

    public String toString() {
        StringBuilder output = new StringBuilder();
        for (int y = 0; y < height; y++) {
            //check whether its an odd or even row for indentation
            if (y % 2 == 1) output.append(" ");
            for (int x = 0; x < width; x++) {
                if (isFilled(x,y)) {
                    output.append("x ");
                } else {
                    output.append("_ ");
                }
            }
            output.append("\n");
        }
        return output.toString();
    }

    public boolean canSpawnUnit(Unit unit) {
        Coordinate pivotSpawnPoint = unit.getSpawnPoint(width);
        return canPlaceUnit(pivotSpawnPoint, unit);
    }

    public boolean canPlaceUnit(Coordinate pivotPoint, Unit unit) {
        return canPlaceUnit(pivotPoint, unit, new ArrayList<VisitedState>());
    }

    public boolean canPlaceUnit(Coordinate pivotPoint, Unit unit, List<VisitedState> visited) {
        if (visited.contains(new VisitedState(pivotPoint, unit))) return false;
        List<Coordinate> coordinates = unit.getAbsoluteMembers(pivotPoint);
        for (Coordinate coordinate : coordinates) {
            if (isOutside(coordinate)) return false;
            if (isFilled(coordinate)) return false;
        }
        return true;
    }

    private boolean isOutside(Coordinate coordinate) {
        return coordinate.x < 0 || coordinate.x >= width || coordinate.y < 0 || coordinate.y >= height;
    }

    public int applyLocking(Unit unit, Coordinate unitPlace, int rotated) {
        int points = 0;
        for (Coordinate member : unit.getAbsoluteMembers(unitPlace, rotated)) {
            assert !isFilled(member);
            filled[cellIndex(member)] = true;
            points++;
        }
        int linesCleared = clearFullLines();
        points += 50 * (1 + linesCleared) * linesCleared;
        if (linesClearedOld > 1) points += Math.floor((linesClearedOld - 1) * points / 10);
        linesClearedOld = linesCleared;
        return points;
    }
    
    public int calculatePotentialPoints(Unit unit, Coordinate unitPlace, int rotated) {
        boolean[] backupFilled = Arrays.copyOf(filled, filled.length);
    	int backupLinesClearedOld = linesClearedOld;
    	int points = applyLocking(unit, unitPlace, rotated);
    	linesClearedOld = backupLinesClearedOld;
    	System.arraycopy(backupFilled, 0, filled, 0, filled.length);
    	return points;
    }

    private int clearFullLines() {
        int linesCleared = 0;
        for (int y = 0; y < height; y++) {
            boolean lineFull = true;
            for (int x = 0; x < width; x++) {
                if (!isFilled(x, y)) {
                    lineFull = false;
                    break;
                }
            }
            if (lineFull) {
                clearLine(y);
                linesCleared ++;
            }
        }
        return linesCleared;
    }

    private void clearLine(int y) {
        System.arraycopy(filled, 0, filled, width, y * width);
        Arrays.fill(filled, 0, width, false);
    }


    public int getWidth() {
        return width;
    }

    public List<Command> getNonLockingMoves(Unit unit, Coordinate position, List<VisitedState> visited) {
        List<Command> possible = new ArrayList<>();
        if (canPlaceUnit(position.move(Command.SOUTHEAST), unit)) possible.add(Command.SOUTHEAST);
        if (canPlaceUnit(position.move(Command.SOUTHWEST), unit)) possible.add(Command.SOUTHWEST);
        if (canPlaceUnit(position.move(Command.EAST), unit, visited)) possible.add(Command.EAST);
        if (canPlaceUnit(position.move(Command.WEST), unit, visited)) possible.add(Command.WEST);
        if (canPlaceUnit(position, unit.getRotatedUnit(1), visited)) possible.add(Command.CLOCKWISE);
        if (canPlaceUnit(position, unit.getRotatedUnit(5), visited)) possible.add(Command.COUNTERCLOCKWISE);
        return possible;
    }

    public int getHeight() {
        return height;
    }
    

    public boolean doesFillRow(Coordinate position, Unit currentUnit) {
        List<Coordinate> coordinates = currentUnit.getAbsoluteMembers(position);
        for (Coordinate coordinate : coordinates) {
            if (isOutside(coordinate)) return false;
            if (isFilled(coordinate)) return false;
        }
        for (Coordinate coordinate : coordinates) {
            boolean full = true;
            int y = coordinate.y;
            for (int x = 0; x < width; x++) {
                if (!(isFilled(x, y) || inUnit(coordinates, x, y))) {
                    full = false;
                    break;
                }
            }
            if (full) {
                return true;
            }
        }
        return false;
    }

    private static boolean inUnit(List<Coordinate> cs, int x, int y) {
        for (Coordinate c : cs) {
            if (c.x == x && c.y == y) {
                return true;
            }
        }
        return false;
    }
	
 public List<Command> getLockingMoves(Unit unit, Coordinate position) {
	 //we do not need to check visited locations, because we only take positions which are invalid, thus we cannot have been there
	 List<Command> possible = new ArrayList<>();
     if (!canPlaceUnit(position.move(Command.SOUTHEAST), unit)) possible.add(Command.SOUTHEAST);
     if (!canPlaceUnit(position.move(Command.SOUTHWEST), unit)) possible.add(Command.SOUTHWEST);
     if (!canPlaceUnit(position.move(Command.EAST), unit)) possible.add(Command.EAST);
     if (!canPlaceUnit(position.move(Command.WEST), unit)) possible.add(Command.WEST);
     if (!canPlaceUnit(position, unit.getRotatedUnit(1))) possible.add(Command.CLOCKWISE);
     if (!canPlaceUnit(position, unit.getRotatedUnit(5))) possible.add(Command.COUNTERCLOCKWISE);
     return possible;
	}

 public List<Command> getFillingMoves(Unit unit, Coordinate position) {
	 return getFillingMoves(0, unit, position);
 	}
 
 public List<Command> getFillingMoves(int depth, Unit unit, Coordinate position) {
	 List<Command> possible = new ArrayList<>(6);
	 if (depth == 0) {
		 if (doesFillRow(position.move(Command.SOUTHEAST), unit)) possible.add(Command.SOUTHEAST);
		 if (doesFillRow(position.move(Command.SOUTHWEST), unit)) possible.add(Command.SOUTHWEST);
		 if (doesFillRow(position.move(Command.EAST), unit)) possible.add(Command.EAST);
		 if (doesFillRow(position.move(Command.WEST), unit)) possible.add(Command.WEST);
		 if (doesFillRow(position, unit.getRotatedUnit(1))) possible.add(Command.CLOCKWISE);
		 if (doesFillRow(position, unit.getRotatedUnit(5))) possible.add(Command.COUNTERCLOCKWISE);
	 } else {
		 if (!getFillingMoves(depth - 1, unit, position.move(Command.SOUTHEAST)).isEmpty()) possible.add(Command.SOUTHEAST);
		 if (!getFillingMoves(depth - 1, unit, position.move(Command.EAST)).isEmpty()) possible.add(Command.EAST);
		 if (!getFillingMoves(depth - 1, unit, position.move(Command.SOUTHWEST)).isEmpty()) possible.add(Command.SOUTHWEST);
		 if (!getFillingMoves(depth - 1, unit, position.move(Command.WEST)).isEmpty()) possible.add(Command.WEST);
		 if (!getFillingMoves(depth - 1, unit.getRotatedUnit(1), position).isEmpty()) possible.add(Command.CLOCKWISE);
		 if (!getFillingMoves(depth - 1, unit.getRotatedUnit(5), position).isEmpty()) possible.add(Command.COUNTERCLOCKWISE);
	 }
     return possible;
 	}
 }
