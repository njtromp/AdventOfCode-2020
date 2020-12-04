package nl.njtromp.adventofcode_2020;

public class Infi {
    static int aantalPakjes(int lengteZijkant) {
        return
                3 * lengteZijkant * lengteZijkant + // Middenstuk horizontaal
                4 * sommatie(lengteZijkant - 1) + // Schuine zijden (4 halve driehoeken)
                2 * (lengteZijkant * lengteZijkant); // Rechte stukken tussen de schuive zijden (onder en boven)

    }

    private static int sommatie(int n) {
        return n == 0 ? 0 : n + sommatie(n - 1);
    }

    public static void main(String[] args) {
        int lengte = 1;
        while (aantalPakjes(lengte) < 17493412) {
            lengte++;
        }
        System.out.printf("De minimale lengte van een zijde is: %d\n", lengte);
    }
}
