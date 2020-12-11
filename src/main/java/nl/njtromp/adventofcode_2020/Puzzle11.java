package nl.njtromp.adventofcode_2020;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Puzzle11 {
    public static void main(String[] args) {
        String[] input = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(input));
//        System.out.printf("Answer part 2: %d\n", solvePart2(input));
    }

    static int solvePart1(String[] seats) {
        String[] previousSeats;
        do {
            previousSeats = seats;
            seats = newSeatState(previousSeats);
        } while (!Arrays.toString(previousSeats).equals(Arrays.toString(seats)));
        return Arrays.stream(seats).mapToInt(s -> s.replaceAll("[\\.L]", "").length()).sum();
    }

    static int solvePart2(String[] lines) {
        return -1;
    }

    private static String[] newSeatState(String[] seats) {
        String[] newSeats = new String[seats.length];
        for (int i = 0; i < seats.length; i++) {
            StringBuilder row = new StringBuilder();
            for (int j = 0; j < seats[i].length(); j++) {
                switch (seats[i].charAt(j)) {
                    case 'L' : if (countOccupiedSeats(seats, i, j) == 0) {
                        row.append('#');
                    } else {
                        row.append(seats[i].charAt(j));
                    }
                        break;
                    case '#' : if (countOccupiedSeats(seats, i, j) >= 4) {
                        row.append('L');
                    } else {
                        row.append(seats[i].charAt(j));
                    }
                        break;
                    default:
                        row.append(seats[i].charAt(j));
                }
            }
            newSeats[i] = row.toString();
        }

        return newSeats;
    }

    private static int countOccupiedSeats(String[] seats, int row, int column) {
        int occupiedSeats = getSeat(seats, row - 1, column - 1) +
                getSeat(seats, row - 1, column) +
                getSeat(seats, row - 1, column + 1) +
                getSeat(seats, row, column - 1) +
                getSeat(seats, row, column + 1) +
                getSeat(seats, row + 1, column - 1) +
                getSeat(seats, row + 1, column) +
                getSeat(seats, row + 1, column + 1);
        return occupiedSeats;
    }

    private static int getSeat(String[] seats, int row, int column) {
        if (row >= 0 && row < seats.length && column >= 0 && column < seats[row].length()) {
            return seats[row].charAt(column) == '#' ? 1 : 0;
        } else {
            return 0;
        }
    }

    private static String[] readInput() {
        List<String> lines = new ArrayList<>();
        Scanner inputFile = new Scanner(Puzzle11.class.getResourceAsStream("/input-puzzle11.txt"));
        while (inputFile.hasNextLine()) {
            lines.add(inputFile.nextLine());
        }
        return lines.toArray(new String[lines.size()]);
    }

}
