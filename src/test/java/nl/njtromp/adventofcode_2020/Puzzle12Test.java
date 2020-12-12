package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Puzzle12Test {

    static String[] example = {
            "F10",
            "N3",
            "F7",
            "R90",
            "F11"
    };

    @Test
    void checkExamplePart1() {
        assertEquals(25, Puzzle12.solvePart1(example));
    }

    @Test
    void checkExamplePart2() {
        assertEquals(286, Puzzle12.solvePart2(example));
    }

}
