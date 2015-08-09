package solver;

import main.Boardstate;
import main.Solution;
import units.SourceStream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import commands.CommandPathOptions;

public class TaskSolver {

	private Boardstate board;
    private SourceStream stream;
    private List<String> powerWords;

    public TaskSolver(Boardstate board, SourceStream stream, List<String> powerWords) {
        this.board = board;
        this.stream = stream;
        this.powerWords = new ArrayList<>(powerWords);
    }
    
    private List<PathFinder.Mode> getModes() {
    	return Arrays.asList(PathFinder.Mode.values());
    }

    public Solution solve() {
    	List<TaskSolution> subSolutions = new ArrayList<>();
        CommandChooser chooser = new CommandChooser(powerWords);
    	for (PathFinder.Mode mode : getModes()) {
    		Boardstate newBoard = board.getInitialStateClone();
    		SourceStream newStream = stream.getCopy();
    		TaskSolution subSolution = getSolution(newBoard, newStream, mode);
    		chooser.chooseCharacters(subSolution.options);
    		subSolution.points = subSolution.points + chooser.getPowerWordPoints();
    		subSolution.commands = chooser.getCommandString();
    		subSolution.mode = mode;
    		subSolutions.add(subSolution);
    	}
    	TaskSolution bestSolution = subSolutions.get(0);
    	for (TaskSolution subSolution : subSolutions) {
    		if (subSolution.points > bestSolution.points) {
    			bestSolution = subSolution;
    		}
    	}
    	
        Solution solution = new Solution();
        solution.seed = stream.getSeed();
        solution.commandString = bestSolution.commands;
        solution.points = bestSolution.points;
        solution.mode = bestSolution.mode;
        return solution;
    }

    public TaskSolution getSolution(Boardstate board, SourceStream stream, PathFinder.Mode mode) {
        CommandPathOptions options = new CommandPathOptions();
        int points = 0;
        List<String> unusedWords = new ArrayList<>(powerWords);
        while (nextUnitCanSpawn(board, stream)) {
        	//todo not instantiate the placer each time, reset instead (boardstate changed anyway)
            Unitplacer placer = new Unitplacer(board, stream.popNextUnit(), stream.getRemainingStream());
            placer.calculateCommands(mode, new ArrayList<>(powerWords), unusedWords);
            options.addBranch(placer.getCommandBranch());
            points += placer.getPlacementPoints();
        }
        TaskSolution solution = new TaskSolution();
        solution.options = options;
        solution.points = points;
        return  solution;
    }
    
    public class TaskSolution {
    	public CommandPathOptions options;
    	public int points;
    	public String commands;
    	public PathFinder.Mode mode;
	}

    private boolean nextUnitCanSpawn(Boardstate board, SourceStream stream) {
        return stream.getRemainingCount() > 0 && board.canSpawnUnit(stream.seeNextUnit());
    }
}
