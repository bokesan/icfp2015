package solver;


import commands.CommandBranch;
import main.Boardstate;
import solver.PathFinder.PathResult;
import units.Unit;

import java.util.ArrayList;
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
    	PathResult bestResult = null;
    	int bestPoints = 0;
    	//try different modes, pick best one
    	List<PathResult> results = new ArrayList<>();
    	//todo maybe in parallel
        results.add(new PathFinder(boardstate, unit, remainingUnits).findPath(PathFinder.Mode.FILL_ROWS_1));
        results.add(new PathFinder(boardstate, unit, remainingUnits).findPath(PathFinder.Mode.FILL_ROWS_2));
        results.add(new PathFinder(boardstate, unit, remainingUnits).findPath(PathFinder.Mode.FILL_ROWS_3));
        //results.add(new PathFinder(boardstate, unit, remainingUnits).findPath(PathFinder.Mode.FILL_ROWS_4));
        results.add(new PathFinder(boardstate, unit, remainingUnits).findPath(PathFinder.Mode.CHRIS_PATH));
        results.add(new PathFinder(boardstate, unit, remainingUnits).findPath(PathFinder.Mode.WITH_POWER));
        
        for (PathResult result : results) {
        	int points = boardstate.calculatePotentialPoints(unit, result.unitPlace, result.rotated);
        	if (points > bestPoints || bestResult == null) {
        		bestPoints = points;
        		bestResult = result;
        	}
        }
        
        branch = bestResult.commands;
        placementPoints = boardstate.applyLocking(unit, bestResult.unitPlace, bestResult.rotated);
    }

    public CommandBranch getCommandBranch() {
        return branch;
    }

    public int getPlacementPoints() {
        return placementPoints;
    }
}
