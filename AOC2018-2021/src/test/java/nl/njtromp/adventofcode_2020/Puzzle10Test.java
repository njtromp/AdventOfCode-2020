package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Puzzle10Test {

    static String[] smallExample = {
            "16",
            "10",
            "15",
            "5",
            "1",
            "11",
            "7",
            "19",
            "6",
            "12",
            "4"
    };

    static String[] largeExample = {
            "28",
            "33",
            "18",
            "42",
            "31",
            "14",
            "46",
            "20",
            "48",
            "47",
            "24",
            "23",
            "49",
            "45",
            "19",
            "38",
            "39",
            "11",
            "1",
            "32",
            "25",
            "35",
            "8",
            "17",
            "7",
            "9",
            "4",
            "2",
            "34",
            "10",
            "3"
    };

    @Test
    void checksmallExamplePart1() {
        assertEquals(7*5, Puzzle10.solvePart1(smallExample));
    }

    @Test
    void checkLargeExamplePart1() {
        assertEquals(22*10, Puzzle10.solvePart1(largeExample));
    }

    @Test
    void checkSmallExamplePart2() {
        assertEquals(8, Puzzle10.solvePart2(smallExample));
    }

    @Test
    void checkLargeExamplePart2() {
        assertEquals(19208, Puzzle10.solvePart2(largeExample));
    }

}
