package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class Puzzle01Test {
    @Test
    void checkExample() {
        Integer[] numbers = {1721, 979, 366, 299, 675, 1456};
        assertEquals(514579, Puzzle01.solve(numbers));
    }

}
