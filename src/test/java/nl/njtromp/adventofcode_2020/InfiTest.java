package nl.njtromp.adventofcode_2020;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class InfiTest {
    @Test
    void checkInhoudVoor1() {
        assertEquals(5, Infi.aantalPakjes(1));
    }

    @Test
    void checkInhoudVoor2() {
        assertEquals(24, Infi.aantalPakjes(2));
    }

    @Test
    void checkInhoudVoor3() {
        assertEquals(57, Infi.aantalPakjes(3));
    }

    @Test
    void checkInhoudVoor4() {
        assertEquals(104, Infi.aantalPakjes(4));
    }

    @Test
    void checkInhoudVoor10() {
        assertEquals(680, Infi.aantalPakjes(10));
    }

    @Test
    void checkInhoudVoor25() {
        assertEquals(4325, Infi.aantalPakjes(25));
    }
}
