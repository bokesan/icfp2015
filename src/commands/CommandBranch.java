package commands;

import java.util.ArrayList;
import java.util.List;

public class CommandBranch {
    private List<CommandSequence> options;

    public CommandBranch(CommandSequence commands) {
        options = new ArrayList<>();
        options.add(commands);
    }

    public List<CommandSequence> getOptions() {
        return new ArrayList<>(options);
    }
    
    //todo not only display first option
    public String toString() {
    	CommandSequence seq = options.get(0);
    	StringBuilder output = new StringBuilder();
    	for (Command command : seq.getCommands()) {
    		output.append(command.name()).append(" ");
    	}
    	return output.toString();
    }
}
