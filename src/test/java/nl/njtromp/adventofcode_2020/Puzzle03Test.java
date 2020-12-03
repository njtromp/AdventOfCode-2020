package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class Puzzle03Test {
    @Test
    void checkExamplePart1() {
        assertEquals(7, Puzzle03.solvePart1(exampleMap));
    }

    @Test
    void checkExamplePart2() {
        assertEquals(336, Puzzle03.solvePart2(exampleMap, Puzzle03.slopes));
    }

    @Test
    void checkExamplePart2Single() {
        int[][] slopes = {{1, 2}};
        assertEquals(4, Puzzle03.solvePart2(exampleMapModified, slopes));
    }

    static String[] exampleMap = {
            "..##.......",
            "#...#...#..",
            ".#....#..#.",
            "..#.#...#.#",
            ".#...##..#.",
            "..#.##.....",
            ".#.#.#....#",
            ".#........#",
            "#.##...#...",
            "#...##....#",
            ".#..#...#.#"
    };

    static String[] exampleMapModified = {
            "#.##.......",
            "###########",
            ".#....#..#.",
            "###########",
            ".#...##..#.",
            "###########",
            ".#.#.#....#",
            "###########",
            "#.##...#...",
            "###########",
            ".#..##..#.#"
    };

}
