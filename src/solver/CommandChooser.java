package solver;


import commands.Command;
import commands.CommandBranch;
import commands.CommandPathOptions;
import commands.CommandSequence;

import java.util.List;

public class CommandChooser {

    private List<String> powerWords;
    private int points;
    private String commandString;

    public CommandChooser(List<String> powerWords) {
        this.powerWords = powerWords;
        points = 0;
    }

    public static String setFirstCharacter(CommandSequence sequence) {
        StringBuilder result = new StringBuilder();
        for (Command command : sequence.getCommands()) {
            result.append(command.getPossibleCharacters().get(0));
        }
        return result.toString();
    }

    public void chooseCharacters(CommandPathOptions commandOptions) {
        StringBuilder commandBuilder = new StringBuilder();
        for (CommandBranch branch : commandOptions.getBranches()) {
            //todo change from taking first branch when power words work
            CommandSequence sequence = branch.getOptions().get(0);
            for (Command command : sequence.getCommands()) {
                //todo change from taking first character when power words matter
                commandBuilder.append(command.getPossibleCharacters().get(0));
            }
        }
        //todo calculate points for power words once they matter
        commandString = commandBuilder.toString();
    }

    public int getPowerWordPoints() {
        return points;
    }

    public String getCommandString() {
        return commandString;
    }
}
