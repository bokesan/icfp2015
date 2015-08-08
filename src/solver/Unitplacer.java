package solver;


import commands.CommandBranch;
import main.Boardstate;
import units.Unit;

import java.util.List;

public class Unitplacer {

    private Boardstate boardstate;
    private Unit unit;
    private CommandBranch branch;
    private int placementPoints;
    private List<Unit> remainingUnits;

    public Unitplacer(Boardstate board, Unit unit, List<Unit> remainingUnits) {
        this.boardstate = board;
        this.unit = unit;
        this.remainingUnits = remainingUnits;
        placementPoints = 0;
    }

    public void calculateCommands() {
        PathFinder finder = new PathFinder(boardstate, unit, remainingUnits);
        PathFinder.PathResult result = finder.findPath(PathFinder.Mode.ALTERNATING_DOWN_WITH_ROTATION);
        branch = result.commands;
        placementPoints += boardstate.applyLocking(unit, result.unitPlace, result.rotated);
    }

    public CommandBranch getCommandBranch() {
        return branch;
    }

    public int getPlacementPoints() {
        return placementPoints;
    }
}
