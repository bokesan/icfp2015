package solver;

import units.Coordinate;
import units.Unit;

public class VisitedState {

    public final Coordinate position;
    public final Unit unit;

    public VisitedState(Coordinate position, Unit unit) {
        this.position = position;
        this.unit = unit;
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object instanceof VisitedState) {
            VisitedState visitedState = (VisitedState) object;
            return position.equals(visitedState.position) && unit.equals(visitedState.unit);
        }
        return false;
    }
    
    @Override
    public int hashCode() {
        return 31 * position.hashCode() + unit.hashCode();
    }
}