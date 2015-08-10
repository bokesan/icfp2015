package solver;

import java.util.ArrayList;
import java.util.List;

import commands.Command;
import commands.CommandSequence;
import main.Boardstate;
import units.Coordinate;
import units.Unit;

public class DoPower {
	
	private Boardstate board;
	private CommandSequence commands;
	private List<VisitedState> visited;
	private Coordinate position;
	private Unit currentUnit;
	private int rotation;

	public boolean moved = false;

	public DoPower(Boardstate board, CommandSequence commands, List<VisitedState> visited, Coordinate position, Unit currentUnit, int rotation) {
		this.board = board; this. commands = commands; this. visited = visited; this.position = position; this.currentUnit = currentUnit; this.rotation = rotation;
	}

	public boolean doIfPossible(String string, int lookahead) {
		return doIfPossible(Command.translate(string), lookahead);
	}
	
	private boolean doIfPossible(List<Command> commands, int lookahead) {
        if (canPerform(commands))  {
            doPerform(commands);
            return true;
        } else if (lookahead > 0) {
            for (Command command : board.getNonLockingMoves(currentUnit, position, visited)) {
                commands.add(0, command);
                if (doIfPossible(commands, lookahead - 1)) return true;
                commands.remove(0);
            }
        }
        return false;
	}

	private boolean canPerform(List<Command> commands) {
		Unit testUnit = currentUnit;
		Coordinate testPosition = position;
		List<VisitedState> testVisited = new ArrayList<>(visited);
		for (Command command : commands) {
			testUnit = applyCommand(command, testUnit);
			testPosition = applyCommand(command, testPosition);
			if (!board.canPlaceUnit(testPosition, testUnit, testVisited)) return false;
			testVisited.add(new VisitedState(testPosition, testUnit));
		}
		return true;
	}

	private int applyCommand(Command command, int rotation) {
		if (command == Command.CLOCKWISE) return rotation + 1;
		if (command == Command.COUNTERCLOCKWISE) return rotation - 1;
		return rotation;
	}

	private Coordinate applyCommand(Command command, Coordinate testPosition) {
		if (command == Command.CLOCKWISE || command == Command.COUNTERCLOCKWISE) return testPosition;
		return testPosition.move(command);
	}

	private Unit applyCommand(Command command, Unit testUnit) {
		if (command == Command.CLOCKWISE) return testUnit.getRotatedUnit(1);
		if (command == Command.COUNTERCLOCKWISE) return testUnit.getRotatedUnit(5);
		return testUnit;
	}

	private void doPerform(List<Command> performCommands) {
		moved = true;
		for (Command command : performCommands) {
			this.commands.append(command);
			this.currentUnit = applyCommand(command, currentUnit);
			this.position = applyCommand(command, position);
			this.rotation = applyCommand(command, rotation);
			this.visited.add(new VisitedState(position, currentUnit));
		}
	}

	public Coordinate getNewPosition() {
		return position;
	}

	public Unit getCurrentUnit() {
		return currentUnit;
	}

	public int getNewRotation() {
		return rotation;
	}

}
