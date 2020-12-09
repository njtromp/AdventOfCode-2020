package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Puzzle09Test {

    List<Long> example = List.of(
            35L,
            20L,
            15L,
            25L,
            47L,
            40L,
            62L,
            55L,
            65L,
            95L,
            102L,
            117L,
            150L,
            182L,
            127L,
            219L,
            299L,
            277L,
            309L,
            576L
    );

    @Test
    void checkExamplePart1() {
        assertEquals(127, Puzzle09.solvePart1(example, 5));
    }

    @Test
    void checkExamplePart2() {
        assertEquals(62, Puzzle09.solvePart2(example, 5));
    }

}
