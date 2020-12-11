package nl.njtromp.adventofcode_2020;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Puzzle12 {
    public static void main(String[] args) {
        String[] input = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(input));
//        System.out.printf("Answer part 2: %d\n", solvePart2(input));
    }

    static int solvePart1(String[] lines) {
        return -1;
    }

    static int solvePart2(String[] lines) {
        return -1;
    }

    private static String[] readInput() {
        List<String> lines = new ArrayList<>();
        Scanner inputFile = new Scanner(Puzzle12.class.getResourceAsStream("/input-puzzle12.txt"));
        while (inputFile.hasNextLine()) {
            lines.add(inputFile.nextLine());
        }
        return lines.toArray(new String[lines.size()]);
    }
}
