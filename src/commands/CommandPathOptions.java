package commands;

import java.util.ArrayList;
import java.util.List;

public class CommandPathOptions {

    private List<CommandBranch> commands;

    public CommandPathOptions() {
        commands = new ArrayList<>();
    }

    public void addBranch(CommandBranch branch) {
        commands.add(branch);
    }

    public List<CommandBranch> getBranches() {
        return new ArrayList<>(commands);
    }
}
