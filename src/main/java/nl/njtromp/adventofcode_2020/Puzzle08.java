package nl.njtromp.adventofcode_2020;

import java.util.*;

public class Puzzle08 {
    public static void main(String[] args) {
        String[] input = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(input));
//        System.out.printf("Answer part 2: %d\n", solvePart2(input));
    }

    static int solvePart1(String[] instructions) {
        Set<Integer> allreadyExecuted = new HashSet<>();
        int ip = 0;
        int acc = 0;
        while (!allreadyExecuted.contains(ip)) {
            allreadyExecuted.add(ip);
            switch (instructions[ip].substring(0, 3)) {
                case "nop":
                    ip++;
                    break;
                case "jmp":
                    ip += Integer.parseInt(instructions[ip].substring(4));
                    break;
                case "acc":
                    acc += Integer.parseInt(instructions[ip].substring(4));
                    ip++;
                    break;
                default:
                    System.err.println("Oopsy...");
            }
        }
        return acc;
    }

    static int solvePart2(String[] lines) {
        return -1;
    }

    private static String[] readInput() {
        List<String> lines = new ArrayList<>();
        String fileName = String.format("/input-%s.txt", Puzzle08.class.getSimpleName()).toLowerCase();
        Scanner inputFile = new Scanner(Puzzle08.class.getResourceAsStream(fileName));
        while (inputFile.hasNextLine()) {
            lines.add(inputFile.nextLine());
        }
        return lines.toArray(new String[lines.size()]);
    }
}
