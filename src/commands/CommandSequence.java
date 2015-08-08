package commands;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class CommandSequence {
    private List<Command> commands;

    public CommandSequence() {
        commands = new ArrayList<Command>();
    }

    public CommandSequence append(Command command) {
        commands.add(command);
        return this;
    }

    public CommandSequence append(CommandSequence commandSequence) {
        commands.addAll(commandSequence.getCommands());
        return this;
    }

    public Command getLast() {
        if (commands.isEmpty()) return null;
        return commands.get(commands.size() - 1);
    }

    public List<Command> getCommands() {
        return new ArrayList<Command>(commands);
    }
}
