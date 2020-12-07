package nl.njtromp.adventofcode_2020;

import java.util.*;

public class Puzzle07 {
    static final String SHINY_GOLD = "shiny gold";
    static class Bag {
        final String color;
        final List<Bag> content = new ArrayList<>();
        final Map<Bag, Integer> counts = new HashMap<>();
        boolean leadsToGold;

        public Bag(String color) {
            this.color = color;
        }

        int totalNumberOfBags() {
            return content.stream().mapToInt(b -> counts.get(b) + counts.get(b) * b.totalNumberOfBags()).sum();
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Bag bag = (Bag) o;
            return color.equals(bag.color);
        }

        @Override
        public int hashCode() {
            return color.hashCode();
        }
    }

    static int solvePart1(String[] rules) {
        Map<String, Bag> bags = processRules(rules);
        bags.get(SHINY_GOLD).leadsToGold = true;
        for (Bag bag : bags.values()) {
            checkBag(bags, bag);
        }
        return (int) bags.values().stream().filter(b -> b.leadsToGold && !SHINY_GOLD.equals(b.color)).count();
    }

    static int solvePart2(String[] rules) {
        Map<String, Bag> bags = processRules(rules);
        bags.get(SHINY_GOLD).leadsToGold = true;
        for (Bag bag : bags.values()) {
            checkBag(bags, bag);
        }
        return bags.get(SHINY_GOLD).totalNumberOfBags();
    }

    public static void main(String[] args) {
        String[] rules = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(rules));
        System.out.printf("Answer part 2: %d\n", solvePart2(rules));
    }

    private static boolean checkBag(Map<String, Bag> bags, Bag bag) {
        if (SHINY_GOLD.equals(bag.color)) {
            return true;
        } else {
            if (!bag.leadsToGold) {
                bag.leadsToGold = bag.content.stream()
                        .peek(b -> b.leadsToGold = checkBag(bags, b))
                        // IntelliJ suggests to replace this with anyMatch but that might lead to lazy evaluation
                        // and we need ALL bags to be checked so we don't do that.
                        .filter(b -> b.leadsToGold).count() > 0;
            }
            return bag.leadsToGold;
        }
    }

    private static Map<String, Bag> processRules(String[] rules) {
        Map<String, Bag> bags = new HashMap<>();
        for (String rule : rules) {
            String[] ruleParts = rule.split(" bags contain ");
            String colorOuterBag = ruleParts[0];
            Bag outerBag = getBag(bags, colorOuterBag);
            for (String content : ruleParts[1].split(", ")) {
                String countAndColor = content.split(" bag")[0];
                int marker = countAndColor.indexOf(' ');
                int count = "no".equals(countAndColor.substring(0, marker)) ? 0 : Integer.parseInt(countAndColor.substring(0, marker));
                if (count > 0) {
                    String colorInnerBag = countAndColor.substring(marker + 1);
                    Bag innerBag = getBag(bags, colorInnerBag);
                    outerBag.content.add(innerBag);
                    outerBag.counts.put(innerBag, count);
                }
            }
        }
        return bags;
    }

    private static Bag getBag(Map<String, Bag> bags, String color) {
        Bag bag = bags.get(color);
        if (bag == null) {
            bag = new Bag(color);
            bags.put(color, bag);
        }
        return bag;
    }

    private static String[] readInput() {
        List<String> lines = new ArrayList<>();
        String fileName = String.format("/input-%s.txt", Puzzle07.class.getSimpleName()).toLowerCase();
        Scanner inputFile = new Scanner(Puzzle07.class.getResourceAsStream(fileName));
        while (inputFile.hasNextLine()) {
            lines.add(inputFile.nextLine());
        }
        return lines.toArray(new String[lines.size()]);
    }
}
