package units;


import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Unit {

    private final List<Coordinate> members;
    private final Coordinate pivot;
    private UnitDimension dimension;

    @Override
    public boolean equals(Object object) {

        if (object != null && object instanceof Unit) {
            Unit unit = (Unit) object;
            if (!pivot.equals(unit.pivot)) return false;
            //only works cause both lists are sorted
            if (!members.equals(unit.members)) return false;
            return true;
        }
        return false;
    }

    public Unit(JSONObject json) {
        JSONObject pivotJson = json.getJSONObject("pivot");
        this.pivot = new Coordinate(pivotJson.getInt("x"), pivotJson.getInt("y"));
        this.members = new ArrayList<>();
        JSONArray memberJson = json.getJSONArray("members");
        for (int i = 0; i < memberJson.length(); i++) {
            JSONObject member = memberJson.getJSONObject(i);
            this.members.add(new Coordinate(member.getInt("x"), member.getInt("y")));
        }
        setDimension();
    }

    public Unit(Coordinate pivot, List<Coordinate> newMembers) {
        this.members = newMembers;
        this.pivot = pivot;
        setDimension();
    }

    private void setDimension() {
        dimension = new UnitDimension();
        Collections.sort(members);
        for (Coordinate member : members) {
            if (member.x < dimension.minX) dimension.minX = member.x;
            if (member.x > dimension.maxX) dimension.maxX = member.x;
            if (member.y < dimension.minY) dimension.minY = member.y;
            if (member.y > dimension.maxY) dimension.maxY = member.y;
        }
    }

    public static List<Unit> fromJson(JSONArray jsonUnits) {
        List<Unit> units = new ArrayList<>();
        for (int i = 0; i < jsonUnits.length(); i++) {
            JSONObject unit = jsonUnits.getJSONObject(i);
            units.add(new Unit(unit));
        }
        return units;
    }

    public Coordinate getPivot() {
        return pivot;
    }

    public List<Coordinate> getMembers() {
        return new ArrayList<>(members);
    }

    public Coordinate getSpawnPoint(int boardWidth) {
        int yOffset = dimension.minY;
        int unitWidth = 1 + dimension.maxX - dimension.minX;
        int space = boardWidth - unitWidth;
        int xOffset = (int) Math.floor(space / 2) - dimension.minX;
        return new Coordinate(pivot.x + xOffset, pivot.y + yOffset);
    }

    //todo simplify nested ifs
    public List<Coordinate> getAbsoluteMembers(Coordinate pivotPoint) {
        int yOffset = pivotPoint.y - pivot.y;
        int xOffset = pivotPoint.x - pivot.x;
        List<Coordinate> newCoordinates = new ArrayList<>();
        for (Coordinate old : members) {
            if (pivotPoint.y % 2 == pivot.y % 2) {
                //moving from odd to odd row or from even to even row
                newCoordinates.add(new Coordinate(old.x + xOffset, old.y + yOffset));
            } else if (pivotPoint.y % 2 == 0) {
                //moving from odd to even row
                //subtract 1 from x offset in formerly even rows
                if (old.y % 2 == 0) {
                    newCoordinates.add(new Coordinate(old.x + xOffset - 1, old.y + yOffset));
                } else {
                    newCoordinates.add(new Coordinate(old.x + xOffset, old.y + yOffset));
                }
            } else {
                //moving from even to odd row
                //add one to x offset in formerly odd rows
                if (old.y % 2 == 0) {
                    newCoordinates.add(new Coordinate(old.x + xOffset, old.y + yOffset));
                } else {
                    newCoordinates.add(new Coordinate(old.x + xOffset + 1, old.y + yOffset));
                }
            }
        }
        return newCoordinates;
    }

    public  List<Coordinate> getAbsoluteMembers(Coordinate unitPlace, int rotated) {
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

    public Unit getRotatedUnit(int rotations) {
        List<Coordinate> newMembers = new ArrayList<>();
        for (Coordinate member : members) {
            Coordinate newCoordinate = member;
            for (int i = 0; i < rotations; i++) {
                newCoordinate = computeRot(pivot, newCoordinate);
            }
            newMembers.add(newCoordinate);
        }
        return  new Unit(pivot, newMembers);
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
        Cube d = new Cube(cp.x - cc.x, cp.y - cc.y, cp.z - cc.z);
        Cube r = new Cube(-d.z, -d.x, -d.y);
        Cube p = new Cube(cc.x + r.x, cc.y + r.y, cc.z + r.z);
        return cubeToOffset(p);
    }

    private class Cube {
        public int x;
        public int y;
        public int z;
        public Cube(int x, int y, int z) {
            this.x =x; this.y = y; this.z = z;
        }
    }
}
