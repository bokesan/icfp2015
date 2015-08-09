package solver;


import commands.Command;
import commands.CommandBranch;
import commands.CommandSequence;
import main.Boardstate;
import units.Coordinate;
import units.Unit;

import java.util.ArrayList;
import java.util.List;

public class PathFinder {

    public enum Mode {
    	//todo refactor cap and depth into enum member variables
    	CHRIS_PATH, POWER_NO_WORDS, POWER_NO_WORDS_1, POWER_NO_WORDS_2, POWER_NO_WORDS_3,
    	FILL_ROWS_1, FILL_ROWS_2, FILL_ROWS_3, FILL_ROWS_4, FILL_ROWS_5,
    	WITH_ALL_POWER, WITH_ALL_POWER_1, WITH_ALL_POWER_2, WITH_ALL_POWER_3, WITH_ALL_POWER_4,
    	EACH_WORD_ONCE, EACH_WORD_ONCE_1, EACH_WORD_ONCE_2, EACH_WORD_ONCE_3, EACH_WORD_ONCE_4,
    	WAP_CAP_3, WAP_CAP_6, WAP_CAP_7, WAP_CAP_8, WAP_CAP_9, WAP_CAP_10,
    	WAP_1_CAP_3, WAP_1_CAP_6, WAP_1_CAP_7, WAP_1_CAP_8, WAP_1_CAP_9, WAP_1_CAP_10,
    	WAP_2_CAP_3, WAP_2_CAP_6, WAP_2_CAP_7, WAP_2_CAP_8, WAP_2_CAP_9, WAP_2_CAP_10,
    	WAP_3_CAP_3, WAP_3_CAP_6, WAP_3_CAP_7, WAP_3_CAP_8, WAP_3_CAP_9, WAP_3_CAP_10,
    	EWO_CAP_3, EWO_CAP_6, EWO_CAP_7, EWO_CAP_8, EWO_CAP_9, EWO_CAP_10,
    	EWO_1_CAP_3, EWO_1_CAP_6, EWO_1_CAP_7, EWO_1_CAP_8, EWO_1_CAP_9, EWO_1_CAP_10,
    	EWO_2_CAP_3, EWO_2_CAP_6, EWO_2_CAP_7, EWO_2_CAP_8, EWO_2_CAP_9, EWO_2_CAP_10,
    	EWO_3_CAP_3, EWO_3_CAP_6, EWO_3_CAP_7, EWO_3_CAP_8, EWO_3_CAP_9, EWO_3_CAP_10;
    	
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
            case CHRIS_PATH: 		return chrisPath();
            case POWER_NO_WORDS:	return withPower(0, 0);
            case POWER_NO_WORDS_1:	return withPower(1, 0);
            case POWER_NO_WORDS_2:	return withPower(2, 0);
            case POWER_NO_WORDS_3:	return withPower(3, 0);
            case FILL_ROWS_1: 		return fillRows(1);
            case FILL_ROWS_2: 		return fillRows(2);
            case FILL_ROWS_3: 		return fillRows(3);
            case FILL_ROWS_4: 		return fillRows(4);
            case FILL_ROWS_5: 		return fillRows(5);
            case WITH_ALL_POWER: 	return withPower(0, -1);
            case WITH_ALL_POWER_1: 	return withPower(1, -1);
            case WITH_ALL_POWER_2: 	return withPower(2, -1);
            case WITH_ALL_POWER_3: 	return withPower(3, -1);
            case WITH_ALL_POWER_4: 	return withPower(4, -1);
            case EACH_WORD_ONCE:	return eachWordOnce(0, -1);
            case EACH_WORD_ONCE_1:	return eachWordOnce(1, -1);
            case EACH_WORD_ONCE_2:	return eachWordOnce(2, -1);
            case EACH_WORD_ONCE_3:	return eachWordOnce(3, -1);
            case EACH_WORD_ONCE_4:	return eachWordOnce(4, -1);
            case WAP_CAP_3:			return withPower(0, 3);
            case WAP_CAP_6:			return withPower(0, 6);
            case WAP_CAP_7:			return withPower(0, 7);
            case WAP_CAP_8:			return withPower(0, 8);
            case WAP_CAP_9:			return withPower(0, 9);
            case WAP_CAP_10:		return withPower(0, 10);
            case EWO_CAP_3:			return eachWordOnce(0, 3);
            case EWO_CAP_6:			return eachWordOnce(0, 6);
            case EWO_CAP_7:			return eachWordOnce(0, 7);
            case EWO_CAP_8:			return eachWordOnce(0, 8);
            case EWO_CAP_9:			return eachWordOnce(0, 9);
            case EWO_CAP_10:		return eachWordOnce(0, 10);
            case EWO_1_CAP_3: return eachWordOnce(1, 3);
            case EWO_1_CAP_6: return eachWordOnce(1, 6);
            case EWO_1_CAP_7: return eachWordOnce(1, 7);
            case EWO_1_CAP_8: return eachWordOnce(1, 8);
            case EWO_1_CAP_9: return eachWordOnce(1, 9);
            case EWO_1_CAP_10: return eachWordOnce(1, 10);
            case EWO_2_CAP_3: return eachWordOnce(2, 3);
            case EWO_2_CAP_6: return eachWordOnce(2, 6);
            case EWO_2_CAP_7: return eachWordOnce(2, 7);
            case EWO_2_CAP_8: return eachWordOnce(2, 8);
            case EWO_2_CAP_9: return eachWordOnce(2, 9);
            case EWO_2_CAP_10: return eachWordOnce(2, 10);
            case EWO_3_CAP_3: return eachWordOnce(3, 3);
            case EWO_3_CAP_6: return eachWordOnce(3, 6);
            case EWO_3_CAP_7: return eachWordOnce(3, 7);
            case EWO_3_CAP_8: return eachWordOnce(3, 8);
            case EWO_3_CAP_9: return eachWordOnce(3, 9);
            case EWO_3_CAP_10: return eachWordOnce(3, 10);
            case WAP_1_CAP_3: return withPower(1, 3);
            case WAP_1_CAP_6: return withPower(1, 6);
            case WAP_1_CAP_7: return withPower(1, 7);
            case WAP_1_CAP_8: return withPower(1, 8);
            case WAP_1_CAP_9: return withPower(1, 9);
            case WAP_1_CAP_10: return withPower(1, 10);
            case WAP_2_CAP_3: return withPower(2, 3);
            case WAP_2_CAP_6: return withPower(2, 6);
            case WAP_2_CAP_7: return withPower(2, 7);
            case WAP_2_CAP_8: return withPower(2, 8);
            case WAP_2_CAP_9: return withPower(2, 9);
            case WAP_2_CAP_10: return withPower(2, 10);
            case WAP_3_CAP_3: return withPower(3, 3);
            case WAP_3_CAP_6: return withPower(3, 6);
            case WAP_3_CAP_7: return withPower(3, 7);
            case WAP_3_CAP_8: return withPower(3, 8);
            case WAP_3_CAP_9: return withPower(3, 9);
            case WAP_3_CAP_10: return withPower(3, 10);
            default:    throw new IllegalArgumentException("Illegal Mode");
        }
    }
    
    private PathResult eachWordOnce(int depth, int maxWordLength) {
    	filterWords(maxWordLength);
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
        	
        	//use unused power word if possible
        	if (!unusedWords.isEmpty()) {
        		DoPower power = new DoPower(board, commands, visited, position, currentUnit, rotation);
        			for (String word : new ArrayList<>(unusedWords)) {
        					boolean used = power.doIfPossible(word);
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
        List<Command> lockMoves = board.getLockingMoves(currentUnit, position);
        commands.append(checkForWords(lockMoves, commands));
        PathResult result = new PathResult();
        result.rotated = rotation;
        result.unitPlace = position;
        result.commands = new CommandBranch(commands);
        return result;
	}
    
    private PathResult withPower(int depth, int maxWordLength) {
    	filterWords(maxWordLength);
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
        	
        	//use power word if possible
        	DoPower power = new DoPower(board, commands, visited, position, currentUnit, rotation);
        	for (String word : powerWords) {
        		boolean used = power.doIfPossible(word);
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
        List<Command> lockMoves = board.getLockingMoves(currentUnit, position);
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

	private Command checkForWords(List<Command> lockMoves, CommandSequence commands) {
		//todo get power words from args
		List<List<Command>> patterns = new ArrayList<>();
		List<List<Command>> possible = new ArrayList<>();
		patterns.add(Command.translate("cthulhu"));
		patterns.add(Command.translate("ei!"));
		patterns.add(Command.translate("ia! ia!"));
		patterns.add(Command.translate("r'lyeh"));
		patterns.add(Command.translate("yuggoth"));
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
		return lockMoves.get(0);
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
