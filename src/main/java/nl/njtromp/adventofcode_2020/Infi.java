package nl.njtromp.adventofcode_2020;

import java.util.Arrays;

public class Infi {
    static long aantalPakjes(long lengteZijkant) {
        return
                3 * lengteZijkant * lengteZijkant + // Middenstuk horizontaal
                4 * sommatie(lengteZijkant) +       // Schuine zijden (4 halve driehoeken)
                2 * lengteZijkant * lengteZijkant;  // Rechte stukken tussen de schuine zijden (onder en boven)

    }

    private static long sommatie(long n) {
        return (n-1)*n/2;
    }

    private static long bepaalLengte(long aantalInwoners) {
        long lengte = 1;
        while (aantalPakjes(lengte) < aantalInwoners) {
            lengte++;
        }
        return lengte;
    }

    public static void main(String[] args) {
        long lengte = bepaalLengte(17_493_412);
        System.out.printf("De minimale lengte van een zijde is: %d\n", lengte);
        long[] aantalInwoners = {42_732_096L, 369_030_498L, 430_839_868L, 747_685_826L, 1_340_952_816L, 4_541_536_619L};
        System.out.printf("Totaal aantal lappen stof: %d\n", Arrays.stream(aantalInwoners).map(Infi::bepaalLengte).sum() * 8);
    }

}
