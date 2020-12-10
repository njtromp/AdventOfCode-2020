package nl.njtromp.adventofcode_2020;

import java.util.*;
import java.util.stream.Collectors;

public class Puzzle10 {
    public static void main(String[] args) {
        String[] input = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(input));
//        System.out.printf("Answer part 2: %d\n", solvePart2(input));
    }

    static int solvePart1(String[] lines) {
        List<Integer> adepters = Arrays.stream(lines).map(Integer::parseInt).collect(Collectors.toList());
        adepters.add(0);
        Collections.sort(adepters);
        adepters.add(adepters.get(adepters.size() - 1) + 3);
        Map<Integer, Integer> diffs = new HashMap<>();
        for (int i = 0; i < adepters.size() - 1; i++) {
            int joltsDiff = adepters.get(i + 1) - adepters.get(i);
            diffs.put(joltsDiff, 1 + diffs.getOrDefault(joltsDiff, 0));
        }
        return diffs.get(1) * diffs.get(3);
    }

    static int solvePart2(String[] lines) {
        return -1;
    }

    private static String[] readInput() {
        List<String> lines = new ArrayList<>();
        Scanner inputFile = new Scanner(Puzzle10.class.getResourceAsStream("/input-puzzle10.txt"));
        while (inputFile.hasNextLine()) {
            lines.add(inputFile.nextLine());
        }
        return lines.toArray(new String[lines.size()]);
    }
}
