package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Puzzle08Test {

    static String[] example = {
            "nop +0",
            "acc +1",
            "jmp +4",
            "acc +3",
            "jmp -3",
            "acc -99",
            "acc +1",
            "jmp -4",
            "acc +6"
    };

    @Test
    void checkExamplePart1() {
        assertEquals(5, Puzzle08.solvePart1(example));
    }

    @Disabled("Enable after part 1 has been solved!")
    @Test
    void checkExamplePart2() {
        assertEquals(0, Puzzle08.solvePart2(example));
    }

}
