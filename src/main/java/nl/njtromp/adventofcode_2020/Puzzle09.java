package nl.njtromp.adventofcode_2020;

import java.util.*;
import java.util.stream.Collectors;

public class Puzzle09 {
    public static void main(String[] args) {
        List<Long> input = Arrays.stream(readInput()).map(Long::parseLong).collect(Collectors.toList());
        System.out.printf("Answer part 1: %d\n", solvePart1(input, 25));
        System.out.printf("Answer part 2: %d\n", solvePart2(input, 25));
    }

    static long solvePart1(List<Long> input, int preamble) {
        LinkedList<Long> numbers = new LinkedList<>();
        for (long number : input) {
            if (numbers.size() == preamble) {
                if (!canBeConstructes(number, numbers)) {
                    return number;
                }
            }
            numbers.addLast(number);
            if (numbers.size() > preamble) {
                numbers.removeFirst();
            }
        }
        return -1;
    }

    static long solvePart2(List<Long> input, int preamble) {
        final long toMatch = solvePart1(input, preamble);
        int min = 0;
        int max = min;
        long remainder = toMatch;
        while (remainder > 0) {
            while (remainder > 0) {
                remainder -= input.get(max);
                max++;
            }
            if (remainder == 0) {
                return input.subList(min, max).stream().min(Long::compareTo).get() + input.subList(min, max).stream().max(Long::compareTo).get();
            }
            remainder = toMatch;
            min++;
            max = min;
        }
        return -1L;
    }

    private static boolean canBeConstructes(long number, LinkedList<Long> numbers) {
        for (long i : numbers) {
            if (numbers.contains(number - i) && number != i) {
                return true;
            }
        }
        return false;
    }

    private static String[] readInput() {
        List<String> lines = new ArrayList<>();
        Scanner inputFile = new Scanner(Puzzle09.class.getResourceAsStream("/2020/input-puzzle09.txt"));
        while (inputFile.hasNextLine()) {
            lines.add(inputFile.nextLine());
        }
        return lines.toArray(new String[lines.size()]);
    }
}
