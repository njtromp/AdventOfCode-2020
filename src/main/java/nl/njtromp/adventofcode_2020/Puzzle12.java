package nl.njtromp.adventofcode_2020;

import java.util.*;

public class Puzzle12 {
    public static void main(String[] args) {
        String[] input = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(input));
        System.out.printf("Answer part 2: %d\n", solvePart2(input));
    }

    static int solvePart1(String[] lines) {
        int heading = 0;
        int x = 0;
        int y = 0;
        for (String line : lines) {
            if (line.charAt(0) == 'N') {
                y += Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'S') {
                y -= Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'E') {
                x += Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'W') {
                x -= Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'L') {
                heading = Math.abs(360 + heading + Integer.parseInt(line.substring(1))) % 360;
            } else if (line.charAt(0) == 'R') {
                heading = Math.abs(360 + heading - Integer.parseInt(line.substring(1))) % 360;
            } else if (line.charAt(0) == 'F') {
                switch (heading) {
                    case 0 :
                        x += Integer.parseInt(line.substring(1));
                        break;
                    case 90 :
                        y += Integer.parseInt(line.substring(1));
                        break;
                    case 180 :
                        x -= Integer.parseInt(line.substring(1));
                        break;
                    case 270 :
                        y -= Integer.parseInt(line.substring(1));
                        break;
                    default:
                        System.out.printf("Damn it not a heading [%d] I can handle...\n", heading);
                }
            } else {
                System.out.printf("Unknown instruction [%s]!\n", line);
            }
        }
        return Math.abs(x) + Math.abs(y);
    }

    static int solvePart2(String[] lines) {
        int x = 0;
        int y = 0;
        int wx = 10;
        int wy = 1;
        for (String line : lines) {
            if (line.charAt(0) == 'N') {
                wy += Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'S') {
                wy -= Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'E') {
                wx += Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'W') {
                wx -= Integer.parseInt(line.substring(1));
            } else if (line.charAt(0) == 'L') {
                for (int i = 0; i < Integer.parseInt(line.substring(1)) / 90; i++) {
                    int temp = wy;
                    wy = wx;
                    wx = -temp;
                }
            } else if (line.charAt(0) == 'R') {
                for (int i = 0; i < Integer.parseInt(line.substring(1)) / 90; i++) {
                    int temp = wy;
                    wy = -wx;
                    wx = temp;
                }
            } else if (line.charAt(0) == 'F') {
                x += Integer.parseInt(line.substring(1)) * wx;
                y += Integer.parseInt(line.substring(1)) * wy;
            } else {
                System.out.printf("Unknown instruction [%s]!\n", line);
            }
        }
        return Math.abs(x) + Math.abs(y);
    }

    private static String[] readInput() {
        List<String> lines = new ArrayList<>();
        Scanner inputFile = new Scanner(Puzzle12.class.getResourceAsStream("/2020/input-puzzle12.txt"));
        while (inputFile.hasNextLine()) {
            lines.add(inputFile.nextLine());
        }
        return lines.toArray(new String[lines.size()]);
    }
}
