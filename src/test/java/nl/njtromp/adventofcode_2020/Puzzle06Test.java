package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class Puzzle06Test {

    @Test
    void checkExamplePart1() {
        String[] answers = {
                "abc",
                "",
                "a",
                "b",
                "c",
                "",
                "ab",
                "ac",
                "",
                "a",
                "a",
                "a",
                "a",
                "",
                "b"
        };

        assertEquals(11, Puzzle06.solvePart1(answers));
    }

    @Test
    void checkExamplePart2() {
        String[] answers = {
                "abc",
                "",
                "a",
                "b",
                "c",
                "",
                "ab",
                "ac",
                "",
                "a",
                "a",
                "a",
                "a",
                "",
                "b"
        };

        assertEquals(6, Puzzle06.solvePart2(answers));
    }
}
