package commands;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public enum Command {
    EAST, WEST, SOUTHEAST, SOUTHWEST, CLOCKWISE, COUNTERCLOCKWISE;

    private List<Character> possibleCharacters;

    static {
        EAST.possibleCharacters = Arrays.asList('b', 'c', 'e', 'f', 'y', '2');
        WEST.possibleCharacters = Arrays.asList('p', '\'', '!', '.', '0', '3');
        SOUTHEAST.possibleCharacters = Arrays.asList('l', 'm', 'n', 'o', ' ', '5');
        SOUTHWEST.possibleCharacters = Arrays.asList('a', 'g', 'h', 'i', 'j', '4');
        CLOCKWISE.possibleCharacters = Arrays.asList('d', 'q', 'r', 'v', 'z', '1');
        COUNTERCLOCKWISE.possibleCharacters = Arrays.asList('k', 's', 't', 'u', 'w', 'x');
    }

    public List<Character> getPossibleCharacters() {
        return new ArrayList<>(possibleCharacters);
    }
}
