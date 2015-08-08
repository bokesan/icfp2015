package main;

import commands.Command;
import org.json.JSONArray;
import org.json.JSONObject;
import solver.VisitedState;
import units.Coordinate;
import units.Unit;

import java.util.ArrayList;
import java.util.List;


public class Boardstate {

    private int width;
    private int height;
    private boolean[][] filled;
    private int linesClearedOld = 0;

    public Boardstate(int width, int height, boolean[][] filled) {
        this.width = width;
        this.height = height;
        this.filled = filled;
        checkDimensions();
    }

    private void checkDimensions() {
        assert filled.length == width;
        assert filled[0].length == height;
    }

    public Boardstate(JSONObject json) {
        this.height = json.getInt("height");
        this.width = json.getInt("width");
        this.filled = convertFilled(json.getJSONArray("filled"));
        checkDimensions();
    }

    private boolean[][] convertFilled(JSONArray filledJson) {
        boolean[][] filled = new boolean[width][height];
        for (int i = 0; i < filledJson.length(); i++) {
            JSONObject point = filledJson.getJSONObject(i);
            int x = point.getInt("x");
            int y = point.getInt("y");
            filled[x][y] = true;
        }
        return filled;
    }

    public boolean isFilled(int x, int y) {
        return filled[x][y];
    }

    private boolean isFilled(Coordinate coordinate) {
        return isFilled(coordinate.x, coordinate.y);
    }

    public String toString() {
        StringBuilder output = new StringBuilder();
        for (int y = 0; y < filled[0].length; y++) {
            //check whether its an odd or even row for indentation
            if (y % 2 == 1) output.append(" ");
            for (int x = 0; x < filled.length; x++) {
                if (filled[x][y]) {
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
            filled[member.x][member.y] = true;
            points++;
        }
        int linesCleared = clearFullLines();
        points += 50 * (1 + linesCleared) * linesCleared;
        if (linesClearedOld > 1) points += Math.floor((linesClearedOld - 1) * points / 10);
        linesClearedOld = linesCleared;
        return points;
    }
    
    public int calculatePotentialPoints(Unit unit, Coordinate unitPlace, int rotated) {
    	boolean[][] backupFilled = clone2DArray(filled);
    	int backupLinesClearedOld = linesClearedOld;
    	int points = applyLocking(unit, unitPlace, rotated);
    	linesClearedOld = backupLinesClearedOld;
    	filled = backupFilled;
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
        //track back up again, replacing each line with the contents of the previous one
        for (int line = y; y > 0; y--) {
            for (int x = 0; x < width; x++) {
                filled[x][y] = filled[x][y-1];
            }
        }
        //replacing topmost line with empty fields
        for (int x = 0; x < width; x++) {
            filled[x][0] = false;
        }
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
		boolean[][] withUnitApplied = clone2DArray(filled);
		List<Integer> usedY = new ArrayList<>();
        List<Coordinate> coordinates = currentUnit.getAbsoluteMembers(position);
        for (Coordinate coordinate : coordinates) {
            if (isOutside(coordinate)) return false;
            if (isFilled(coordinate)) return false;
            withUnitApplied[coordinate.x][coordinate.y] = true;
            if (!usedY.contains(coordinate.y)) usedY.add(coordinate.y);
        }
        outer: for (int y : usedY) {
        	for (int x = 0; x < width; x++) {
        		if (!withUnitApplied[x][y]) continue outer;
        	}
        	return true;
        }
        return false;
	}
	
	public static boolean[][] clone2DArray(boolean[][] array) {
	    int rows=array.length;
	    //clone the 'shallow' structure of array
	    boolean[][] newArray =(boolean[][]) array.clone();
	    //clone the 'deep' structure of array
	    for(int row=0;row<rows;row++){
	        newArray[row]=(boolean[]) array[row].clone();
	    }
	    return newArray;
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
	 List<Command> possible = new ArrayList<>();
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
