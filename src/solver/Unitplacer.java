package solver;


import commands.CommandBranch;
import main.Boardstate;
import solver.PathFinder.PathResult;
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

    public void calculateCommands(PathFinder.Mode mode, List<String> powerWords, List<String> unusedWords) {
    	PathResult result = new PathFinder(boardstate, unit, remainingUnits, powerWords, unusedWords).findPath(mode);
        branch = result.commands;
        placementPoints = boardstate.applyLocking(unit, result.unitPlace, result.rotated);
    }

    public CommandBranch getCommandBranch() {
        return branch;
    }

    public int getPlacementPoints() {
        return placementPoints;
    }
}
