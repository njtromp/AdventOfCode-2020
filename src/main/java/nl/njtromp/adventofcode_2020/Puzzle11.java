package nl.njtromp.adventofcode_2020;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Puzzle11 {
    public static void main(String[] args) {
        String[] input = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(input));
        System.out.printf("Answer part 2: %d\n", solvePart2(input));
    }

    static int solvePart1(String[] seats) {
        String[] previousSeats;
        do {
            previousSeats = seats;
            seats = newSeatState(previousSeats);
        } while (!Arrays.toString(previousSeats).equals(Arrays.toString(seats)));
        return Arrays.stream(seats).mapToInt(s -> s.replaceAll("[\\.L]", "").length()).sum();
    }

    static int solvePart2(String[] seats) {
        String[] previousSeats;
        do {
            previousSeats = seats;
            seats = newSeatStateLines(previousSeats);
        } while (!Arrays.toString(previousSeats).equals(Arrays.toString(seats)));
        return Arrays.stream(seats).mapToInt(s -> s.replaceAll("[\\.L]", "").length()).sum();
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

    private static String[] newSeatStateLines(String[] seats) {
        String[] newSeats = new String[seats.length];
        for (int i = 0; i < seats.length; i++) {
            StringBuilder row = new StringBuilder();
            for (int j = 0; j < seats[i].length(); j++) {
                switch (seats[i].charAt(j)) {
                    case 'L' : if (countSeatLines(seats, i, j) == 0) {
                        row.append('#');
                    } else {
                        row.append(seats[i].charAt(j));
                    }
                        break;
                    case '#' : if (countSeatLines(seats, i, j) >= 5) {
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

    private static int countSeatLines(String[] seats, int r, int c) {
        int[][] directions = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}};
        int sameSeats = 0;
        for (int[] direction : directions) {
            boolean proceed = true;
            int length = 1;
            while (proceed && validCoordinate(seats, r + length * direction[0], c + length * direction[1])) {
                if (seats[r + length * direction[0]].charAt(c + length * direction[1]) == '#') {
                    sameSeats++;
                    proceed = false;
                } else if (seats[r + length * direction[0]].charAt(c + length * direction[1]) == '.') {
                    length++;
                } else {
                    proceed = false;
                }
            }
        }
        return sameSeats;
    }

    private static boolean validCoordinate(String[] seats, int r, int c) {
        return r >= 0 && c >= 0 && r < seats.length && c < seats[r].length();
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
