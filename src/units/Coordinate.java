package units;

import commands.Command;

public class Coordinate implements Comparable<Coordinate> {

    public final int x;
    public final int y;

    public Coordinate(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Coordinate move(Command command) {
        switch (command) {
            case SOUTHWEST: return new Coordinate(x - ((y + 1) % 2), y + 1);
            case SOUTHEAST: return new Coordinate(x + (y % 2), y + 1);
            case WEST:      return new Coordinate(x - 1, y);
            case EAST:      return new Coordinate(x + 1 , y);
            default:        return this;
        }
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object instanceof Coordinate) {
            Coordinate coordinate = (Coordinate) object;
            return this.x == coordinate.x && this.y == coordinate.y;
        }
        return false;
    }

    @Override
    public int compareTo(Coordinate other) {
        int result = x - other.x;
        if (result == 0) {
            result = y - other.y;
        }
        return result;
    }
    
    public String toString() {
    	return "(" + x + "," + y + ")";
    }
}