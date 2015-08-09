package solver;

import commands.Command;
import commands.CommandPathOptions;
import commands.CommandSequence;
import main.Boardstate;
import main.Solution;
import solver.TaskSolver.TaskSolution;
import units.SourceStream;

import java.util.ArrayList;
import java.util.List;


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
    	List<PathFinder.Mode> modes = new ArrayList<>();
    	modes.add(PathFinder.Mode.FILL_ROWS_3);
    	modes.add(PathFinder.Mode.WITH_ALL_POWER_1);
    	modes.add(PathFinder.Mode.WITH_ALL_POWER_2);
    	modes.add(PathFinder.Mode.EACH_WORD_ONCE);
    	return modes;
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
	}

    private boolean nextUnitCanSpawn(Boardstate board, SourceStream stream) {
        return stream.getRemainingCount() > 0 && board.canSpawnUnit(stream.seeNextUnit());
    }
}
