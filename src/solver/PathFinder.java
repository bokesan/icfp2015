package solver;


import commands.Command;
import commands.CommandBranch;
import commands.CommandSequence;
import main.Boardstate;
import units.Coordinate;
import units.Unit;

import java.security.PublicKey;
import java.util.ArrayList;
import java.util.List;

public class PathFinder {

    public enum Mode {DOWN_NO_ROTATION, ALTERNATING_DOWN, ALTERNATING_DOWN_WITH_ROTATION, CHRIS_PATH, FILL_ROWS_1, FILL_ROWS_2, FILL_ROWS_3, FILL_ROWS_4, WITH_POWER}

    private Boardstate board;
    private final Unit unit;
    List<Unit> remainingUnits;

    public PathFinder(Boardstate board, Unit unit, List<Unit> remainingUnits) {
        this.board = board;
        this.unit = unit;
        this.remainingUnits = remainingUnits;
    }

    public PathResult findPath(Mode mode) {
        switch (mode) {
            case DOWN_NO_ROTATION: return justGoDown();
            case ALTERNATING_DOWN: return alternatingDown();
            case ALTERNATING_DOWN_WITH_ROTATION: return alternatingDownWithRotation();
            case CHRIS_PATH: return chrisPath();
            case FILL_ROWS_1: return fillRows(1);
            case FILL_ROWS_2: return fillRows(2);
            case FILL_ROWS_3: return fillRows(3);
            case FILL_ROWS_4: return fillRows(4);
            case WITH_POWER: return withPower();
            default:    throw new IllegalArgumentException("Illegal Mode");
        }
    }
    
    private PathResult withPower() {
		return fillRows(0);
	}

	private PathResult fillRows(int depth) {
        List<VisitedState> visited = new ArrayList<>();
        Unit currentUnit = unit;
        int rotation = 0;
        CommandSequence commands = new CommandSequence();
        Coordinate position = unit.getSpawnPoint(board.getWidth());
        visited.add(new VisitedState(position, currentUnit));
        List<Command> moves = board.getNonLockingMoves(unit, position, visited);
        outer: while (!moves.isEmpty()) {
        	//if we can fill a row, lock the unit in place
        	if (board.doesFillRow(position, currentUnit)) {
        		List<Command> lockingMoves = board.getLockingMoves(currentUnit, position);
        		//there is always at least one, if we are in a full row (EAST or WEST)
        		commands.append(lockingMoves.get(0));
        		PathResult result = new PathResult();
                result.rotated = rotation;
                result.unitPlace = position;
                result.commands = new CommandBranch(commands);
                return result;
        	} 
        	
        	//check whether we can fill a row next move
        	List<Command> fillingMoves = board.getFillingMoves(depth, currentUnit, position);
        	if (!fillingMoves.isEmpty()) {
        		//check whether one of the filling moves is allowed
        		List<Command> allowed = new ArrayList<>();
        		for (Command command : fillingMoves) {
        			if (moves.contains(command)) allowed.add(command);
        		}
        		if (!allowed.isEmpty()) moves = allowed; //only allow filling moves if possible
        	} else {
        		for (Coordinate member : currentUnit.getAbsoluteMembers(position)) {
        			if (member.y == board.getHeight() - 1) break outer;
        		}
        	}

        	//go through possible moves, sorted by priority
            if (moves.contains(Command.SOUTHWEST)) {
                commands.append(Command.SOUTHWEST);
                position = position.move(Command.SOUTHWEST);
            } else if (moves.contains(Command.SOUTHEAST)) {
                commands.append(Command.SOUTHEAST);
                position = position.move(Command.SOUTHEAST);
            } else if (moves.contains(Command.WEST)) {
                commands.append(Command.WEST);
                position = position.move(Command.WEST);
            } else if (moves.contains(Command.EAST)) {
                commands.append(Command.EAST);
                position = position.move(Command.EAST);
            } else if (moves.contains(Command.CLOCKWISE)) {
                commands.append(Command.CLOCKWISE);
                currentUnit = currentUnit.getRotatedUnit(1);
                rotation ++;
            } else if (moves.contains(Command.COUNTERCLOCKWISE)) {
                commands.append(Command.COUNTERCLOCKWISE);
                currentUnit = currentUnit.getRotatedUnit(5);
                rotation --;
            } else {
                break outer;
            }
            visited.add(new VisitedState(position, currentUnit));
            moves = board.getNonLockingMoves(currentUnit, position, visited);
        }
        //is always valid, cannot turn up an error, will just lock the unit
        commands.append(Command.SOUTHWEST);
        PathResult result = new PathResult();
        result.rotated = rotation;
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
    }

    private PathResult chrisPath() {
        List<VisitedState> visited = new ArrayList<>();
        Unit currentUnit = unit;
        int rotation = 0;
        CommandSequence commands = new CommandSequence();
        Coordinate position = unit.getSpawnPoint(board.getWidth());
        visited.add(new VisitedState(position, currentUnit));
        List<Command> moves = board.getNonLockingMoves(unit, position, visited);
        outer: while (!moves.isEmpty()) {
            for (Coordinate member : currentUnit.getAbsoluteMembers(position)) {
                if (member.y == board.getHeight() - 1) break outer;
            }
            if (moves.contains(Command.SOUTHWEST)) {
                commands.append(Command.SOUTHWEST);
                position = position.move(Command.SOUTHWEST);
            } else if (moves.contains(Command.SOUTHEAST)) {
                commands.append(Command.SOUTHEAST);
                position = position.move(Command.SOUTHEAST);
            } else if (moves.contains(Command.WEST)) {
                commands.append(Command.WEST);
                position = position.move(Command.WEST);
            } else if (moves.contains(Command.EAST)) {
                commands.append(Command.EAST);
                position = position.move(Command.EAST);
            } else if (moves.contains(Command.CLOCKWISE)) {
                commands.append(Command.CLOCKWISE);
                currentUnit = currentUnit.getRotatedUnit(1);
                rotation ++;
            } else if (moves.contains(Command.COUNTERCLOCKWISE)) {
                commands.append(Command.COUNTERCLOCKWISE);
                currentUnit = currentUnit.getRotatedUnit(5);
                rotation --;
            } else {
                break outer;
            }
            visited.add(new VisitedState(position, currentUnit));
            moves = board.getNonLockingMoves(currentUnit, position, visited);
        }
        //is always valid, cannot turn up an error, will just lock the unit
        commands.append(Command.SOUTHWEST);
        PathResult result = new PathResult();
        result.rotated = rotation;
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
    }

    //todo un-nest ifs
    private PathResult alternatingDownWithRotation() {
        List<VisitedState> visited = new ArrayList<>();
        Unit currentUnit = unit;
        int rotation = 0;
        CommandSequence commands = new CommandSequence();
        Coordinate position = unit.getSpawnPoint(board.getWidth());
        visited.add(new VisitedState(position, currentUnit));
        List<Command> moves = board.getNonLockingMoves(unit, position, visited);
        while (!moves.isEmpty()) {
            if (remainingUnits.size() % 2 == 0) {
                if (moves.contains(Command.SOUTHWEST)) {
                    commands.append(Command.SOUTHWEST);
                    position = position.move(Command.SOUTHWEST);
                } else if (moves.contains(Command.SOUTHEAST)) {
                    commands.append(Command.SOUTHEAST);
                    position = position.move(Command.SOUTHEAST);
                } else if (moves.contains(Command.EAST)) {
                    commands.append(Command.EAST);
                    position = position.move(Command.EAST);
                } else if (moves.contains(Command.WEST)) {
                    commands.append(Command.WEST);
                    position = position.move(Command.WEST);
                } else if (moves.contains(Command.CLOCKWISE)) {
                    commands.append(Command.CLOCKWISE);
                    currentUnit = currentUnit.getRotatedUnit(1);
                    rotation ++;
                } else if (moves.contains(Command.COUNTERCLOCKWISE)) {
                    commands.append(Command.COUNTERCLOCKWISE);
                    currentUnit = currentUnit.getRotatedUnit(5);
                    rotation --;
                } else {
                    break;
                }
            } else {
                if (moves.contains(Command.SOUTHEAST)) {
                    commands.append(Command.SOUTHEAST);
                    position = position.move(Command.SOUTHEAST);
                } else if (moves.contains(Command.SOUTHWEST)) {
                    commands.append(Command.SOUTHWEST);
                    position = position.move(Command.SOUTHWEST);
                } else if (moves.contains(Command.WEST)) {
                    commands.append(Command.WEST);
                    position = position.move(Command.WEST);
                } else if (moves.contains(Command.EAST)) {
                    commands.append(Command.EAST);
                    position = position.move(Command.EAST);
                } else if (moves.contains(Command.COUNTERCLOCKWISE)) {
                    commands.append(Command.COUNTERCLOCKWISE);
                    currentUnit = currentUnit.getRotatedUnit(5);
                    rotation --;
                } else if (moves.contains(Command.CLOCKWISE)) {
                    commands.append(Command.CLOCKWISE);
                    currentUnit = currentUnit.getRotatedUnit(1);
                    rotation ++;
                } else {
                    break;
                }
            }
            visited.add(new VisitedState(position, currentUnit));
            moves = board.getNonLockingMoves(currentUnit, position, visited);
        }
        //is always valid, cannot turn up an error, will just lock the unit
        commands.append(Command.SOUTHWEST);
        PathResult result = new PathResult();
        result.rotated = rotation;
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
    }

    private PathResult alternatingDown() {
        if (remainingUnits.size() % 2 == 0) return  justGoDown();
        CommandSequence commands = new CommandSequence();
        Coordinate position = unit.getSpawnPoint(board.getWidth());
        List<Command> moves = board.getNonLockingMoves(unit, position, new ArrayList<VisitedState>());
        while (!moves.isEmpty()) {
            if (moves.contains(Command.SOUTHWEST)) {
                commands.append(Command.SOUTHWEST);
                position = position.move(Command.SOUTHWEST);
            } else if (moves.contains(Command.SOUTHEAST)) {
                commands.append(Command.SOUTHEAST);
                position = position.move(Command.SOUTHEAST);
            } else if (moves.contains(Command.EAST)) {
                commands.append(Command.EAST);
                position = position.move(Command.EAST);
            } else if (moves.contains(Command.WEST)) {
                commands.append(Command.WEST);
                position = position.move(Command.WEST);
            } else {
                break;
            }
            moves = board.getNonLockingMoves(unit, position, new ArrayList<VisitedState>());
        }
        //is always valid, cannot turn up an error, will just lock the unit
        commands.append(Command.SOUTHWEST);
        PathResult result = new PathResult();
        result.rotated = 0; //we did not rotate
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
    }

    private PathResult justGoDown() {
        CommandSequence commands = new CommandSequence();
        Coordinate position = unit.getSpawnPoint(board.getWidth());
        List<Command> moves = board.getNonLockingMoves(unit, position, new ArrayList<VisitedState>());
        while (!moves.isEmpty()) {
            if (moves.contains(Command.SOUTHEAST)) {
                commands.append(Command.SOUTHEAST);
                position = position.move(Command.SOUTHEAST);
            } else if (moves.contains(Command.SOUTHWEST)) {
                commands.append(Command.SOUTHWEST);
                position = position.move(Command.SOUTHWEST);
            } else {
                break;
            }
            moves = board.getNonLockingMoves(unit, position, new ArrayList<VisitedState>());
        }
        //is always valid, cannot turn up an error, will just lock the unit
        commands.append(Command.SOUTHEAST);
        PathResult result = new PathResult();
        result.rotated = 0; //we did not rotate
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
    }

    public class PathResult {
        public Coordinate unitPlace;
        public int rotated;
        public CommandBranch commands;
    }
}
