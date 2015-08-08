package units;


public class RandomGenerator {

    private static final int multiplier = 1103515245;
    private static final int increment = 12345;

    private int current;

    public static int[] getNumbers(int seed, int length) {
        int[] numbers = new int[length];
        RandomGenerator random = new RandomGenerator(seed);
        numbers[0] = (seed >> 16) & 32767;
        for (int i = 1; i < numbers.length; i++) {
            numbers[i] = random.getNext();
        }
        return numbers;
    }

    private RandomGenerator(int seed) {
        current = seed;
    }

    private int getNext() {
        current = ((current * multiplier) + increment);
        return (current >> 16) & 32767;
    }
}
