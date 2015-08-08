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
}
