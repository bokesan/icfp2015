package main;

import commands.Command;

import org.json.JSONArray;
import org.json.JSONObject;

import solver.VisitedState;
import units.Coordinate;
import units.Unit;

import java.util.BitSet;
import java.util.EnumSet;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;


public class Boardstate {

    private final int width;
    private final int height;
    
    private final BitSet[] filled;
    
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

    private BitSet[] convertFilled(JSONArray filledJson) {
        BitSet[] filled = new BitSet[height];
        for (int i = 0; i < height; i++) {
            filled[i] = new BitSet(width);
        }
        int n = filledJson.length();
        for (int i = 0; i < n; i++) {
            JSONObject point = filledJson.getJSONObject(i);
            int x = point.getInt("x");
            int y = point.getInt("y");
            filled[y].set(x);
        }
        return filled;
    }

    /**
     * Cell filled?
     * <p>
     * <b>Caution:</b> throws Exception when coordinated out of board.
     */
    private boolean isFilled(int x, int y) {
        return filled[y].get(x);
    }

    private boolean isFilled(Coordinate coordinate) {
        return isFilled(coordinate.x, coordinate.y);
    }
    
    private void setFilled(int x, int y, boolean value) {
        filled[y].set(x, value);
    }
    
    private void setFilled(Coordinate c, boolean value) {
        setFilled(c.x, c.y, value);
    }
    
    private boolean isFull(int row) {
        return filled[row].cardinality() == width;
    }
    
    private int freeInRow(int row) {
        return width - filled[row].cardinality();
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
        return isValidPosition(unit, pivotSpawnPoint);
    }

    public boolean canPlaceUnit(Coordinate pivotPoint, Unit unit, List<VisitedState> visited) {
        if (visitedContains(visited, pivotPoint, unit)) return false;
        return isValidPosition(unit, pivotPoint);
    }
    
    private static boolean visitedContains(List<VisitedState> visited, Coordinate c, Unit u) {
        for (VisitedState st : visited) {
            if (st.position.equals(c) && st.unit.equals(u)) {
                return true;
            }
        }
        return false;
    }

    private boolean isOutside(Coordinate coordinate) {
        return coordinate.x < 0 || coordinate.x >= width || coordinate.y < 0 || coordinate.y >= height;
    }

    public int applyLocking(Unit unit, Coordinate unitPlace, int rotated) {
        int points = 0;
        for (Coordinate member : unit.getAbsoluteMembers(unitPlace, rotated)) {
            assert !isFilled(member);
            setFilled(member, true);
            points++;
        }
        int linesCleared = clearFullLines();
        points += 50 * (1 + linesCleared) * linesCleared;
        if (linesClearedOld > 1) points += Math.floor((linesClearedOld - 1) * points / 10.0);
        linesClearedOld = linesCleared;
        return points;
    }
    
    
    private int clearFullLines() {
        int linesCleared = 0;
        for (int y = 0; y < height; y++) {
            if (isFull(y)) {
                clearLine(y);
                linesCleared ++;
            }
        }
        return linesCleared;
    }

    private void clearLine(int y) {
        for (int i = y; i > 0; i--) {
            filled[i] = filled[i-1];
        }
        filled[0] = new BitSet(width);
    }


    public int getWidth() {
        return width;
    }

    public EnumSet<Command> getNonLockingMoves(Unit unit, Coordinate position, List<VisitedState> visited) {
        EnumSet<Command> possible = EnumSet.noneOf(Command.class);
        if (isValidPosition(unit, position.move(Command.SOUTHEAST))) possible.add(Command.SOUTHEAST);
        if (isValidPosition(unit, position.move(Command.SOUTHWEST))) possible.add(Command.SOUTHWEST);
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
        Coordinate[] coordinates = currentUnit.getAbsoluteMembers(position);
        if (!isValidPosition(coordinates)) {
            return false;
        }
        int unitSize = currentUnit.size();
        for (Coordinate coordinate : coordinates) {
            int y = coordinate.y;
            if (unitSize >= freeInRow(y)) {
                boolean full = true;
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
        }
        return false;
    }

    private static boolean inUnit(Coordinate[] cs, int x, int y) {
        for (Coordinate c : cs) {
            if (c.x == x && c.y == y) {
                return true;
            }
        }
        return false;
    }
	
    public EnumSet<Command> getLockingMoves(Unit unit, Coordinate position) {
        //we do not need to check visited locations, because we only take positions which are invalid, thus we cannot have been there
        EnumSet<Command> possible = EnumSet.noneOf(Command.class);
        if (!isValidPosition(unit, position.move(Command.SOUTHEAST))) possible.add(Command.SOUTHEAST);
        if (!isValidPosition(unit, position.move(Command.SOUTHWEST))) possible.add(Command.SOUTHWEST);
        if (!isValidPosition(unit, position.move(Command.EAST))) possible.add(Command.EAST);
        if (!isValidPosition(unit, position.move(Command.WEST))) possible.add(Command.WEST);
        if (!isValidPosition(unit.getRotatedUnit(1), position)) possible.add(Command.CLOCKWISE);
        if (!isValidPosition(unit.getRotatedUnit(5), position)) possible.add(Command.COUNTERCLOCKWISE);
        return possible;
    }

    
    public EnumSet<Command> getFillingMoves(final int depth, final Unit unit, final Coordinate position) {
        EnumSet<Command> possible = EnumSet.noneOf(Command.class);
        if (!(fillableRows(unit) && isValidPosition(unit, position))) {
            return possible;
        }
        ExecutorService exec = Main.getThreadPool();
        if (depth <= 0) {
            if (doesFillRow(position.move(Command.SOUTHEAST), unit)) possible.add(Command.SOUTHEAST);
            if (doesFillRow(position.move(Command.SOUTHWEST), unit)) possible.add(Command.SOUTHWEST);
            if (doesFillRow(position.move(Command.EAST), unit)) possible.add(Command.EAST);
            if (doesFillRow(position.move(Command.WEST), unit)) possible.add(Command.WEST);
            if (doesFillRow(position, unit.getRotatedUnit(1))) possible.add(Command.CLOCKWISE);
            if (doesFillRow(position, unit.getRotatedUnit(5))) possible.add(Command.COUNTERCLOCKWISE);
        } else if (exec != null) {
            Future<Boolean> fSE = exec.submit(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    return hasFillingMoves(depth-1, unit, position.move(Command.SOUTHEAST));
                }
            });
            Future<Boolean> fE = exec.submit(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    return hasFillingMoves(depth-1, unit, position.move(Command.EAST));
                }
            });
            Future<Boolean> fSW = exec.submit(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    return hasFillingMoves(depth-1, unit, position.move(Command.SOUTHWEST));
                }
            });
            Future<Boolean> fW = exec.submit(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    return hasFillingMoves(depth-1, unit, position.move(Command.WEST));
                }
            });
            Future<Boolean> fCW = exec.submit(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    return hasFillingMoves(depth-1, unit.getRotatedUnit(1), position);
                }
            });
            Future<Boolean> fCCW = exec.submit(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    return hasFillingMoves(depth-1, unit.getRotatedUnit(5), position);
                }
            });
            try {
                if (fSE.get()) possible.add(Command.SOUTHEAST);
                if (fE.get()) possible.add(Command.EAST);
                if (fSW.get()) possible.add(Command.SOUTHWEST);
                if (fW.get()) possible.add(Command.WEST);
                if (fCW.get()) possible.add(Command.CLOCKWISE);
                if (fCCW.get()) possible.add(Command.COUNTERCLOCKWISE);
            } catch (ExecutionException | InterruptedException ex) {
                throw new RuntimeException("bad thread thingie", ex);
            }
        } else {
            if (hasFillingMoves(depth - 1, unit, position.move(Command.SOUTHEAST))) possible.add(Command.SOUTHEAST);
            if (hasFillingMoves(depth - 1, unit, position.move(Command.EAST))) possible.add(Command.EAST);
            if (hasFillingMoves(depth - 1, unit, position.move(Command.SOUTHWEST))) possible.add(Command.SOUTHWEST);
            if (hasFillingMoves(depth - 1, unit, position.move(Command.WEST))) possible.add(Command.WEST);
            if (hasFillingMoves(depth - 1, unit.getRotatedUnit(1), position)) possible.add(Command.CLOCKWISE);
            if (hasFillingMoves(depth - 1, unit.getRotatedUnit(5), position)) possible.add(Command.COUNTERCLOCKWISE);
        }
        return possible;
    }
    
    /**
     * Are there any rows this unit could possibly fill?
     */
    private boolean fillableRows(Unit unit) {
        int size = unit.size();
        for (int y = height - 1; y >= 0; y--) {
            if (size >= freeInRow(y)) {
                return true;
            }
        }
        return false;
    }

    private boolean hasFillingMoves(int depth, Unit unit, Coordinate position) {
        if (!isValidPosition(unit, position)) {
            return false;
        }
        if (depth <= 0) {
            return ((doesFillRow(position.move(Command.SOUTHEAST), unit)) ||
                    (doesFillRow(position.move(Command.SOUTHWEST), unit)) ||
                    (doesFillRow(position.move(Command.EAST), unit)) ||
                    (doesFillRow(position.move(Command.WEST), unit)) ||
                    (doesFillRow(position, unit.getRotatedUnit(1))) ||
                    (doesFillRow(position, unit.getRotatedUnit(5))));
        } else {
            return ((hasFillingMoves(depth - 1, unit, position.move(Command.SOUTHEAST))) ||
                    (hasFillingMoves(depth - 1, unit, position.move(Command.SOUTHWEST))) ||
                    (hasFillingMoves(depth - 1, unit, position.move(Command.EAST))) ||
                    (hasFillingMoves(depth - 1, unit, position.move(Command.WEST))) ||
                    (hasFillingMoves(depth - 1, unit.getRotatedUnit(1), position)) ||
                    (hasFillingMoves(depth - 1, unit.getRotatedUnit(5), position)));
        }
    }

    private boolean isValidPosition(Coordinate[] members) {
        for (Coordinate c : members) {
            if (isOutside(c) || isFilled(c)) {
                return false;
            }
        }
        return true;
    }

    private boolean isValidPosition(Unit unit, Coordinate position) {
        return isValidPosition(unit.getAbsoluteMembers(position));
    }
 }
