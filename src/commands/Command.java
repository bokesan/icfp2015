package commands;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;

public enum Command {
    EAST("ebcfy2"),
    WEST("!p'.03"),
    SOUTHEAST("lmno 5"),
    SOUTHWEST("iaghj4"),
    CLOCKWISE("dqrvz1"),
    COUNTERCLOCKWISE("kstuwx");

    private final String possibleCharacters;
    
    private Command(String chars) {
        possibleCharacters = chars;
    }

    public String getPossibleCharacters() {
        return possibleCharacters;
    }
    
    public char getDefaultCharacter() {
        return possibleCharacters.charAt(0);
    }
    
    public static List<Command> translate(String powerWord) {
    	List<Command> commands = new ArrayList<>(powerWord.length());
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
	
	public static Command getAny(EnumSet<Command> cmds) {
	    Iterator<Command> it = cmds.iterator();
	    if (it.hasNext()) {
	        return it.next();
	    }
	    return null;
	}
}
