package solver;


import commands.Command;
import commands.CommandBranch;
import commands.CommandPathOptions;
import commands.CommandSequence;

import java.util.List;
import java.util.regex.Pattern;


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
    	points = 0;
    	commandString = "";
        StringBuilder commandBuilder = new StringBuilder();
        for (CommandBranch branch : commandOptions.getBranches()) {
            //todo change from taking first branch when power words work
            CommandSequence sequence = branch.getOptions().get(0);
            List<Command> commands = sequence.getCommands();
            while (!commands.isEmpty()) {
            	String chars = getPowerPrefix(commands);
            	commands = commands.subList(chars.length(), commands.size());
            	commandBuilder.append(chars);
            }
        }
        commandString = commandBuilder.toString();
        calculatePowerwordPoints();
    }

    private String getPowerPrefix(List<Command> commands) {
		for (String word : powerWords) {
			if (word.length() > commands.size()) continue;
			List<Command> wordCommands = Command.translate(word);
			if (commands.subList(0, wordCommands.size()).equals(wordCommands)) {
				return word;
			}
		}
		return commands.get(0).getPossibleCharacters().get(0).toString();
	}

	private void calculatePowerwordPoints() {
    	for (String word : powerWords) {
    		points += getPoints(word.length(), countOccurences(commandString, word));
    	}
	}
    
    private int countOccurences(String base, String power) {
    	return base.split(Pattern.quote(power), -1).length - 1;
    }
    
    private int getPoints(int len, int reps) {
    	int points = 2 * len * reps;
    	if (reps > 0) points += 300;
    	return points;
    }

	public int getPowerWordPoints() {
        return points;
    }

    public String getCommandString() {
        return commandString;
    }
}
