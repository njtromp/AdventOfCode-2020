package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class Puzzle02Test {
    @Test
    void checkExamplePart1() {
        String[] passwordList = {"1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"};
        assertEquals(2, Puzzle02.solvePart1(passwordList));
    }

    @Test
    void knownValidPasswordsPart1() {
        String[] passwordList = {"11-12 n: nnndnnnnnnnn"};
        assertEquals(1, Puzzle02.solvePart1(passwordList));
    }

    @Test
    void checkExamplePart2() {
        String[] passwordList = {"1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"};
        assertEquals(1, Puzzle02.solvePart2(passwordList));
    }

}
