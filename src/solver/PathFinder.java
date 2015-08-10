package solver;


import commands.Command;
import commands.CommandBranch;
import commands.CommandSequence;
import main.Boardstate;
import units.Coordinate;
import units.Unit;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;

public class PathFinder {

    public enum Mode {
    	//todo refactor cap and depth into enum member variables
        
        //level 0
        WITH_ALL_POWER, EACH_WORD_ONCE, WAP_CAP_7, WA1_WITH_ALL_POWER, EACH_WORD_ONCE_1, WA1_EWO, WA1_WAP_CAP_6, WA1_WAP_CAP_8, 
        //level 1
        WITH_ALL_POWER_2, WA2_WITH_ALL_POWER, WA1_WITH_ALL_POWER_1, WA2_EWO, WA1_EWO1, WA3_WITH_ALL_POWER, EACH_WORD_ONCE_3, WA3_WAP_CAP_8, WA2_EWO1, 
        //level 2
        WA2_EWO2, WA1_EWO3, WA2_EWO3, WA4_WITH_ALL_POWER, WA1_WITH_ALL_POWER_4,
        //level 3
        WITH_ALL_POWER_5, EACH_WORD_ONCE_5, 
        //level 4
        EACH_WORD_ONCE_6, WA2_EACH_WORD_ONCE_6, WA6_EWO6, WA6_WAP6;
        
        public static List<Mode> getModes(int level) {
            switch (level) {
            case 0:     return Arrays.asList(WITH_ALL_POWER, EACH_WORD_ONCE, WAP_CAP_7, WA1_WITH_ALL_POWER, EACH_WORD_ONCE_1, WA1_EWO, WA1_WAP_CAP_6, WA1_WAP_CAP_8);
            case 1:     return Arrays.asList(WITH_ALL_POWER_2, WA2_WITH_ALL_POWER, WA1_WITH_ALL_POWER_1, WA2_EWO, WA1_EWO1, WA3_WITH_ALL_POWER, EACH_WORD_ONCE_3, WA3_WAP_CAP_8, WA2_EWO1);
            case 2:     return Arrays.asList(WA2_EWO2, WA1_EWO3, WA2_EWO3, WA4_WITH_ALL_POWER, WA1_WITH_ALL_POWER_4);
            case 3:     return Arrays.asList(WITH_ALL_POWER_5, EACH_WORD_ONCE_5);
            case 4:     return Arrays.asList(EACH_WORD_ONCE_6, WA2_EACH_WORD_ONCE_6, WA6_EWO6, WA6_WAP6);
            default:    return Arrays.asList(EACH_WORD_ONCE_6, WA2_EACH_WORD_ONCE_6, WA6_EWO6, WA6_WAP6);
            }
        }
        
        public static int getMaxLevel() {
            return 4; //chosen by fair dice roll
        }
        
    	public String toString() { return name(); }
    }

    private Boardstate board;
    private final Unit unit;
    private List<String> powerWords;
    private List<String> unusedWords;
    List<Unit> remainingUnits;

    public PathFinder(Boardstate board, Unit unit, List<Unit> remainingUnits, List<String> powerWords, List<String> unusedWords) {
        this.board = board;
        this.unit = unit;
        this.powerWords = powerWords;
        this.remainingUnits = remainingUnits;
        this.unusedWords = unusedWords;
    }

    //todo extract strategy methods, de-duplicate code
    //todo refactor cap and depth into variable in enum
    public PathResult findPath(Mode mode) {
        switch (mode) {
            //level 0 modes
            case WITH_ALL_POWER: 	    return withPower(0, -1, 0);
            case EACH_WORD_ONCE:	    return eachWordOnce(0, -1, 0);
            case WAP_CAP_7:			    return withPower(0, 7, 0);
            case WA1_WITH_ALL_POWER:    return withPower(0, -1, 1);
            case EACH_WORD_ONCE_1:	    return eachWordOnce(1, -1, 0);
            case WA1_EWO:			    return eachWordOnce(0, -1, 1);
            case WA1_WAP_CAP_6:         return withPower(0, 6, 1);
            case WA1_WAP_CAP_8:         return withPower(0, 8, 1);
            
            //level 1 modes
            case WITH_ALL_POWER_2: 	    return withPower(2, -1, 0);
            case WA2_WITH_ALL_POWER:    return withPower(0, -1, 2);
            case WA1_WITH_ALL_POWER_1:  return withPower(1, -1, 1);
            case WA2_EWO:               return eachWordOnce(0, -1, 2);
            case WA1_EWO1:              return eachWordOnce(1, -1, 1);
            case WA3_WITH_ALL_POWER:    return withPower(0, -1, 3);
            case EACH_WORD_ONCE_3:	    return eachWordOnce(3, -1, 0);
            case WA2_EWO1:              return eachWordOnce(1, -1, 2);
            case WA3_WAP_CAP_8:         return withPower(0, 8, 3);
            
            //level 2 modes
            case WA2_EWO2:              return eachWordOnce(2, -1, 2);
            case WA1_EWO3:              return eachWordOnce(3, -1, 1);
            case WA2_EWO3:              return eachWordOnce(3, -1, 2);
            case WA4_WITH_ALL_POWER:    return withPower(0, -1, 4);
            case WA1_WITH_ALL_POWER_4:  return withPower(4, -1, 1);
            
            //level 3 modes
            case WITH_ALL_POWER_5:      return withPower(5, -1, 0);
            case EACH_WORD_ONCE_5:      return eachWordOnce(5, -1, 0);
            
            //level 4 modes
            case EACH_WORD_ONCE_6:      return eachWordOnce(6, -1, 0);
            case WA2_EACH_WORD_ONCE_6:  return eachWordOnce(6, -1, 2);
            case WA6_EWO6:              return eachWordOnce(6, -1, 6);
            case WA6_WAP6:              return withPower(6, -1, 6);
            
            default:    throw new IllegalArgumentException("Illegal Mode");
        }
    }
    
    private PathResult eachWordOnce(int depth, int maxWordLength, int wordAhead) {
    	filterWords(maxWordLength);
    	List<VisitedState> visited = new ArrayList<>();
        Unit currentUnit = unit;
        int rotation = 0;
        CommandSequence commands = new CommandSequence();
        Coordinate position = unit.getSpawnPoint(board.getWidth());
        visited.add(new VisitedState(position, currentUnit));
        EnumSet<Command> moves = board.getNonLockingMoves(unit, position, visited);
        outer: while (!moves.isEmpty()) {
        	//if we can fill a row, lock the unit in place
        	if (board.doesFillRow(position, currentUnit)) {
        		EnumSet<Command> lockingMoves = board.getLockingMoves(currentUnit, position);
        		//there is always at least one, if we are in a full row (EAST or WEST)
        		commands.append(Command.getAny(lockingMoves));
        		PathResult result = new PathResult();
                result.rotated = rotation;
                result.unitPlace = position;
                result.commands = new CommandBranch(commands);
                return result;
        	} 
        	
        	//check whether we can fill a row next move
        	EnumSet<Command> fillingMoves = board.getFillingMoves(depth, currentUnit, position);
        	if (!fillingMoves.isEmpty()) {
        		//check whether one of the filling moves is allowed
        		EnumSet<Command> allowed = EnumSet.noneOf(Command.class);
        		for (Command command : fillingMoves) {
        			if (moves.contains(command)) allowed.add(command);
        		}
        		if (!allowed.isEmpty()) moves = allowed; //only allow filling moves if possible
        	} else {
        		for (Coordinate member : currentUnit.getAbsoluteMembers(position)) {
        			if (member.y == board.getHeight() - 1) break outer;
        		}
        	}
        	
        	//use unused power word if possible
        	if (!unusedWords.isEmpty()) {
        		DoPower power = new DoPower(board, commands, visited, position, currentUnit, rotation);
        			for (String word : new ArrayList<>(unusedWords)) {
        					boolean used = power.doIfPossible(word, wordAhead);
        					if (used) unusedWords.remove(word);
        			}
        		if (power.moved) {
        			position = power.getNewPosition();
        			currentUnit = power.getCurrentUnit();
        			rotation = power.getNewRotation();
        			moves = board.getNonLockingMoves(currentUnit, position, visited);
        		}
        	}

        	

        	//go through possible moves, sorted by priority
            if (moves.contains(Command.SOUTHWEST)) {
            	if (moves.contains(Command.EAST) && moves.contains(Command.SOUTHEAST)) {
            		//replace simple move with ei! powerword if possible
            		commands.append(Command.EAST).append(Command.SOUTHWEST).append(Command.WEST);
            		visited.add(new VisitedState(position.move(Command.EAST), currentUnit));
            		visited.add(new VisitedState(position.move(Command.SOUTHEAST), currentUnit));
            	} else {
            		commands.append(Command.SOUTHWEST);
            	}
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
        //see whether we can snatch a word when locking
        EnumSet<Command> lockMoves = board.getLockingMoves(currentUnit, position);
        commands.append(checkForWords(lockMoves, commands));
        PathResult result = new PathResult();
        result.rotated = rotation;
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
	}
    
    private PathResult withPower(int depth, int maxWordLength, int wordAhead) {
    	filterWords(maxWordLength);
    	List<VisitedState> visited = new ArrayList<>();
        Unit currentUnit = unit;
        int rotation = 0;
        CommandSequence commands = new CommandSequence();
        Coordinate position = unit.getSpawnPoint(board.getWidth());
        visited.add(new VisitedState(position, currentUnit));
        EnumSet<Command> moves = board.getNonLockingMoves(unit, position, visited);
        outer: while (!moves.isEmpty()) {
        	//if we can fill a row, lock the unit in place
        	if (board.doesFillRow(position, currentUnit)) {
        		EnumSet<Command> lockingMoves = board.getLockingMoves(currentUnit, position);
        		//there is always at least one, if we are in a full row (EAST or WEST)
        		commands.append(Command.getAny(lockingMoves));
        		PathResult result = new PathResult();
                result.rotated = rotation;
                result.unitPlace = position;
                result.commands = new CommandBranch(commands);
                return result;
        	} 
        	
        	//check whether we can fill a row next move
        	EnumSet<Command> fillingMoves = board.getFillingMoves(depth, currentUnit, position);
        	if (!fillingMoves.isEmpty()) {
        		//check whether one of the filling moves is allowed
        		EnumSet<Command> allowed = EnumSet.noneOf(Command.class);
        		for (Command command : fillingMoves) {
        			if (moves.contains(command)) allowed.add(command);
        		}
        		if (!allowed.isEmpty()) moves = allowed; //only allow filling moves if possible
        	} else {
        		for (Coordinate member : currentUnit.getAbsoluteMembers(position)) {
        			if (member.y == board.getHeight() - 1) break outer;
        		}
        	}
        	
        	//use power word if possible
        	DoPower power = new DoPower(board, commands, visited, position, currentUnit, rotation);
        	for (String word : powerWords) {
        		boolean used = power.doIfPossible(word, wordAhead);
        		if (used) unusedWords.remove(word);
        	}
        	if (power.moved) {
        		position = power.getNewPosition();
        		currentUnit = power.getCurrentUnit();
        		rotation = power.getNewRotation();
        		moves = board.getNonLockingMoves(currentUnit, position, visited);
        	}
        	

        	//go through possible moves, sorted by priority
            if (moves.contains(Command.SOUTHWEST)) {
            	if (moves.contains(Command.EAST) && moves.contains(Command.SOUTHEAST)) {
            		//replace simple move with ei! powerword if possible
            		commands.append(Command.EAST).append(Command.SOUTHWEST).append(Command.WEST);
            		visited.add(new VisitedState(position.move(Command.EAST), currentUnit));
            		visited.add(new VisitedState(position.move(Command.SOUTHEAST), currentUnit));
            	} else {
            		commands.append(Command.SOUTHWEST);
            	}
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
        //see whether we can snatch a word when locking
        EnumSet<Command> lockMoves = board.getLockingMoves(currentUnit, position);
        commands.append(checkForWords(lockMoves, commands));
        PathResult result = new PathResult();
        result.rotated = rotation;
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
	}

	private void filterWords(int maxWordLength) {
		if (maxWordLength < 0) return;
		List<String> allowed = new ArrayList<>(powerWords);
		for (String word : powerWords) {
			if (word.length() > maxWordLength) {
				allowed.remove(word);
				unusedWords.remove(word);
			}
		}
		powerWords = allowed;
	}

	private Command checkForWords(EnumSet<Command> lockMoves, CommandSequence commands) {
		List<List<Command>> patterns = new ArrayList<>();
		for (String word : powerWords) {
		    patterns.add(Command.translate(word));
		}
		List<Command> previous = commands.getCommands();
		outer: for (List<Command> pattern : patterns) {
			if (previous.size() + 1 < pattern.size()) continue;
			if (!lockMoves.contains(pattern.get(pattern.size() - 1))) continue;
			for (int i = 2; i <= pattern.size(); i++) {
				if (!(pattern.get(pattern.size() - i) == previous.get(previous.size() - i + 1))) continue outer;
			}
			return pattern.get(pattern.size() - 1);
		}
		//todo pick one to continue or start a pattern
		return Command.getAny(lockMoves);
	}

	public class PathResult {
        public Coordinate unitPlace;
        public int rotated;
        public CommandBranch commands;
    }
}
