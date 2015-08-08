package commands;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public enum Command {
    EAST, WEST, SOUTHEAST, SOUTHWEST, CLOCKWISE, COUNTERCLOCKWISE;

    private List<Character> possibleCharacters;

    static {
        EAST.possibleCharacters = Arrays.asList('e', 'b', 'c', 'f', 'y', '2');
        WEST.possibleCharacters = Arrays.asList('!', 'p', '\'', '.', '0', '3');
        SOUTHEAST.possibleCharacters = Arrays.asList('l', 'm', 'n', 'o', ' ', '5');
        SOUTHWEST.possibleCharacters = Arrays.asList('i', 'a', 'g', 'h', 'j', '4');
        CLOCKWISE.possibleCharacters = Arrays.asList('d', 'q', 'r', 'v', 'z', '1');
        COUNTERCLOCKWISE.possibleCharacters = Arrays.asList('k', 's', 't', 'u', 'w', 'x');
    }

    public List<Character> getPossibleCharacters() {
        return new ArrayList<>(possibleCharacters);
    }
    
    public static List<Command> translate(String powerWord) {
    	List<Command> commands = new ArrayList<>();
    	for (Character c : powerWord.toCharArray()) {
    		Command command = getCommandForChar(c);
    		if (command != null) commands.add(command);
    	}
    	return commands;
    }

	private static Command getCommandForChar(Character c) {
		switch (c) {
		case 'b':
		case 'c':
		case 'e':
		case 'f':
		case 'y':
		case '2': return EAST;
		case 'p':
		case '\'':
		case '!':
		case '.':
		case '0':
		case '3': return WEST;
		case 'l':
		case 'm':
		case 'n':
		case 'o':
		case ' ':
		case '5': return SOUTHEAST;
		case 'a':
		case 'g':
		case 'h':
		case 'i':
		case 'j':
		case '4': return SOUTHWEST;
		case 'd':
		case 'q':
		case 'r':
		case 'v':
		case 'z':
		case '1': return CLOCKWISE;
		case 'k':
		case 's':
		case 't':
		case 'u':
		case 'w':
		case 'x': return COUNTERCLOCKWISE;
		default: return null;
		}
	}
}
