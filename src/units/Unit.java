package units;


import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Unit {

    private final Coordinate[] members;
    private final Coordinate pivot;
    private final UnitDimension dimension;
    
    @Override
    public boolean equals(Object object) {
        if (object != null && object instanceof Unit) {
            Unit unit = (Unit) object;
            if (!pivot.equals(unit.pivot)) return false;
            // only works cause both lists are sorted
            if (!Arrays.equals(members, unit.members)) return false;
            return true;
        }
        return false;
    }
    
    @Override
    public int hashCode() {
        return 59 * pivot.hashCode() + Arrays.hashCode(members);
    }
    
    /**
     * Number of members.
     */
    public int size() {
        return members.length;
    }
    
    public String toString() {
    	StringBuilder output = new StringBuilder();
    	output.append("pivot: ").append(pivot);
    	output.append(" members: ");
    	String sep = " ";
    	for (Coordinate member : members) {
    		output.append(sep).append(member);
    		sep = ", "; 
    	}
    	return output.toString();
    }

    public Unit(JSONObject json) {
        JSONObject pivotJson = json.getJSONObject("pivot");
        this.pivot = new Coordinate(pivotJson.getInt("x"), pivotJson.getInt("y"));
        JSONArray memberJson = json.getJSONArray("members");
        int numMembers = memberJson.length(); 
        this.members = new Coordinate[numMembers];
        for (int i = 0; i < numMembers; i++) {
            JSONObject member = memberJson.getJSONObject(i);
            this.members[i] = new Coordinate(member.getInt("x"), member.getInt("y"));
        }
        this.dimension = setDimension();
    }

    private Unit(Coordinate pivot, Coordinate[] newMembers) {
        this.members = newMembers;
        this.pivot = pivot;
        this.dimension = setDimension();
    }

    private UnitDimension setDimension() {
        Arrays.sort(members);
        int minX = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxY = Integer.MIN_VALUE;
        for (Coordinate member : members) {
            if (member.x < minX) minX = member.x;
            if (member.x > maxX) maxX = member.x;
            if (member.y < minY) minY = member.y;
            if (member.y > maxY) maxY = member.y;
        }
        return new UnitDimension(minX, maxX, minY, maxY);
    }

    public static List<Unit> fromJson(JSONArray jsonUnits) {
        int n = jsonUnits.length();
        List<Unit> units = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            JSONObject unit = jsonUnits.getJSONObject(i);
            units.add(new Unit(unit));
        }
        return units;
    }

    public Coordinate getPivot() {
        return pivot;
    }

    public Coordinate getSpawnPoint(int boardWidth) {
        int yOffset = dimension.minY;
        int unitWidth = 1 + dimension.maxX - dimension.minX;
        int space = boardWidth - unitWidth;
        int xOffset = space / 2 - dimension.minX;
        return new Coordinate(pivot.x + xOffset, pivot.y + yOffset);
    }

    //todo simplify nested ifs
    public Coordinate[] getAbsoluteMembers(Coordinate pivotPoint) {
        int yOffset = pivotPoint.y - pivot.y;
        int xOffset = pivotPoint.x - pivot.x;
        Coordinate[] newCoordinates = new Coordinate[members.length];
        if ((pivotPoint.y & 1) == (pivot.y & 1)) {
            // moving from odd to odd row or from even to even row
            for (int i = 0; i < members.length; i++) {
                Coordinate old = members[i];
                newCoordinates[i] = new Coordinate(old.x + xOffset, old.y + yOffset);
            }
        } else if ((pivotPoint.y & 1) == 0) {
            // moving from odd to even row
            // subtract 1 from x offset in formerly even rows
            for (int i = 0; i < members.length; i++) {
                Coordinate old = members[i];
                if ((old.y & 1) == 0) {
                    newCoordinates[i] = new Coordinate(old.x + xOffset - 1, old.y + yOffset);
                } else {
                    newCoordinates[i] = new Coordinate(old.x + xOffset, old.y + yOffset);
                }
            }
        } else {
            // moving from even to odd row
            // add one to x offset in formerly odd rows
            for (int i = 0; i < members.length; i++) {
                Coordinate old = members[i];
                if ((old.y & 1) == 0) {
                    newCoordinates[i] = new Coordinate(old.x + xOffset, old.y + yOffset);
                } else {
                    newCoordinates[i] = new Coordinate(old.x + xOffset + 1, old.y + yOffset);
                }
            }
        }
        return newCoordinates;
    }

    public Coordinate[] getAbsoluteMembers(Coordinate unitPlace, int rotated) {
        Unit rotatedUnit = rotate(rotated);
        return rotatedUnit.getAbsoluteMembers(unitPlace);
    }

    private Unit rotate(int rotated) {
        rotated = rotated % 6;
        switch (rotated) {
            case 0: return this;
            case 1:     //FALLTHROUGH
            case -5:    //rotated 60° clock wise
                        return getRotatedUnit(1);
            case 2:     //FALLTHROUGH
            case -4:    //rotated 120° clock wise
                        return getRotatedUnit(2);
            case 3:     //FALLTHROUGH
            case -3:    //rotated 180°
                        return getRotatedUnit(3);
            case 4:     //FALLTHROUGH
            case -2:    //rotated 120° counter clockwise
                        return getRotatedUnit(4);
            case 5:     //FALLTHROUGH
            case -1:    //rotated 60° counter clockwise
                        return getRotatedUnit(5);
            default:    throw new IllegalArgumentException("Illegal rotation: " + rotated);
        }
    }

    // cache rotations
    private final Map<Integer, Unit> rotations = new HashMap<>();
    
    public Unit getRotatedUnit(int rotations) {
        Unit u = this.rotations.get(rotations);
        if (u != null) {
            return u;
        }
        
        Coordinate[] newMembers = new Coordinate[members.length];
        for (int k = 0; k < members.length; k++) {
            Coordinate newCoordinate = members[k];
            for (int i = 0; i < rotations; i++) {
                newCoordinate = computeRot(pivot, newCoordinate);
            }
            newMembers[k] = newCoordinate;
        }
        u = new Unit(pivot, newMembers);
        this.rotations.put(rotations, u);
        return u;
    }

    Coordinate cubeToOffset(Cube cube) {
        int col = (cube.x + (cube.z - (cube.z & 1)) / 2);
        int row = cube.z;
        return new Coordinate(col, row);
    }

    Cube offsetToCube(Coordinate coordinate) {
        int x = (coordinate.x - (coordinate.y - (coordinate.y & 1)) / 2);
        int z = coordinate.y;
        int y = -x - z;
        return new Cube(x, y, z);
    }

    Coordinate computeRot(Coordinate center, Coordinate point) {
        Cube cc = offsetToCube(center);
        Cube cp = offsetToCube(point);
        cp.subtract(cc);
        cp.rot();
        cp.add(cc);
        return cubeToOffset(cp);
    }

    private class Cube {
        private int x;
        private int y;
        private int z;
        
        public Cube(int x, int y, int z) {
            this.x =x; this.y = y; this.z = z;
        }
        
        public void add(Cube c) {
            x += c.x;
            y += c.y;
            z += c.z;
        }
        public void subtract(Cube c) {
            x -= c.x;
            y -= c.y;
            z -= c.z;
        }
        public void rot() {
            int tx = -z;
            int ty = -x;
            int tz = -y;
            x = tx; y = ty; z = tz;
        }
        
    }

    public int getMaxDistFromPivot() {
        int d = 0;
        for (Coordinate c : members) {
            d = Math.max(d, c.distanceTo(pivot));
        }
        return d;
    }
}
