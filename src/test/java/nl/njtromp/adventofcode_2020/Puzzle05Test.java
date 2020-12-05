package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class Puzzle05Test {

    @Test
    void checkExamples() {
        assertEquals(357, Puzzle05.convertToSeatId("FBFBBFFRLR"));
        assertEquals(567, Puzzle05.convertToSeatId("BFFFBBFRRR"));
        assertEquals(119, Puzzle05.convertToSeatId("FFFBBBFRRR"));
        assertEquals(820, Puzzle05.convertToSeatId("BBFFBBFRLL"));
    }

    @Test
    void checkRowDecoding() {
        assertEquals(44, Puzzle05.convertToSeatId("FBFBBFF"));
        assertEquals(70, Puzzle05.convertToSeatId("BFFFBBF"));
        assertEquals(14, Puzzle05.convertToSeatId("FFFBBBF"));
        assertEquals(102, Puzzle05.convertToSeatId("BBFFBBF"));
    }

    @Test
    void checkColumDecoding() {
        assertEquals(5, Puzzle05.convertToSeatId("RLR"));
        assertEquals(7, Puzzle05.convertToSeatId("RRR"));
        assertEquals(7, Puzzle05.convertToSeatId("RRR"));
        assertEquals(4, Puzzle05.convertToSeatId("RLL"));
    }
}
