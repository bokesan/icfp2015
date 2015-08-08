package solver;

import commands.Command;
import commands.CommandPathOptions;
import commands.CommandSequence;
import main.Boardstate;
import main.Solution;
import units.SourceStream;

import java.util.List;


public class TaskSolver {

    private Boardstate board;
    private SourceStream stream;
    private List<String> powerWords;
    private int points;

    public TaskSolver(Boardstate board, SourceStream stream, List<String> powerWords) {
        this.board = board;
        this.stream = stream;
        this.powerWords = powerWords;
    }

    public Solution solve() {
        points = 0;
        CommandPathOptions commandOptions = getOptions();
        Solution solution = new Solution();
        solution.seed = stream.getSeed();
        CommandChooser chooser = new CommandChooser(powerWords);
        chooser.chooseCharacters(commandOptions);
        solution.points = points + chooser.getPowerWordPoints();
        solution.commandString = chooser.getCommandString();
        return solution;
    }

    public CommandPathOptions getOptions() {
        CommandPathOptions options = new CommandPathOptions();
        while (nextUnitCanSpawn()) {
            Unitplacer placer = new Unitplacer(board, stream.popNextUnit(), stream.getRemainingStream());
            placer.calculateCommands();
            options.addBranch(placer.getCommandBranch());
            points += placer.getPlacementPoints();
        }
        return  options;
    }

    private boolean nextUnitCanSpawn() {
        return stream.getRemainingCount() > 0 && board.canSpawnUnit(stream.seeNextUnit());
    }
}
