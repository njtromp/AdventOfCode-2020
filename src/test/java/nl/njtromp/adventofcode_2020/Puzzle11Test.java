package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Puzzle11Test {

    static String[] example = {
            "L.LL.LL.LL",
            "LLLLLLL.LL",
            "L.L.L..L..",
            "LLLL.LL.LL",
            "L.LL.LL.LL",
            "L.LLLLL.LL",
            "..L.L.....",
            "LLLLLLLLLL",
            "L.LLLLLL.L",
            "L.LLLLL.LL"
    };

    @Test
    void checkExamplePart1() {
        assertEquals(37, Puzzle11.solvePart1(example));
    }

    @Test
    void checkExample1Part2() {
        assertEquals(26, Puzzle11.solvePart2(example));
    }

}
