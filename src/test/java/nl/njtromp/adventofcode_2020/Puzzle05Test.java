package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class Puzzle05Test {

    @Test
    void checkExamples() {
        assertEquals(567, Puzzle05.convertToSeatId("BFFFBBFRRR"));
        assertEquals(119, Puzzle05.convertToSeatId("FFFBBBFRRR"));
        assertEquals(820, Puzzle05.convertToSeatId("BBFFBBFRLL"));
    }

}
