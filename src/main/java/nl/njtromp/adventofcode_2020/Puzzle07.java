package nl.njtromp.adventofcode_2020;

import java.util.*;

public class Puzzle07 {
    static final String SHINY_GOLD = "shiny gold";
    static class Bag {
        final String color;
        final List<Bag> content = new ArrayList<>();
        final Map<Bag, Integer> counts = new HashMap<>();
        boolean checked;
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
        prepareBagsForProcessing(bags);
        for (Bag bag : bags.values()) {
            checkbag(bags, bag);
        }
        return (int) bags.values().stream().filter(b -> b.leadsToGold && !SHINY_GOLD.equals(b.color)).count();
    }

    static int solvePart2(String[] rules) {
        Map<String, Bag> bags = processRules(rules);
        prepareBagsForProcessing(bags);
        for (Bag bag : bags.values()) {
            checkbag(bags, bag);
        }
        return bags.get(SHINY_GOLD).totalNumberOfBags();
    }

    private static void prepareBagsForProcessing(Map<String, Bag> bags) {
        bags.values().forEach(b -> {
            b.checked = false;
            b.leadsToGold = false;
        });
        bags.get(SHINY_GOLD).leadsToGold = true;
    }

    private static boolean checkbag(Map<String, Bag> bags, Bag bag) {
        if (SHINY_GOLD.equals(bag.color)) {
            return true;
        } else {
            if (bag.checked) {
                return bag.leadsToGold;
            }
            bag.checked = true;
            bag.leadsToGold = bag.content.stream()
                    .map(b -> {
                        if (!b.checked) {
                            b.leadsToGold = checkbag(bags, b);
                        }
                        return b;
                    })
                    .filter(b -> b.leadsToGold).count() > 0;
            return bag.leadsToGold;
        }
    }

    public static void main(String[] args) {
        String[] rules = readInput();
        System.out.printf("Answer part 1: %d\n", solvePart1(rules));
        System.out.printf("Answer part 2: %d\n", solvePart2(rules));
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
        Scanner input = new Scanner(Puzzle07.class.getResourceAsStream("/input-day07.txt"));
        List<String> rules = new ArrayList<>();
        while (input.hasNextLine()) {
            rules.add(input.nextLine());
        }
        return rules.toArray(new String[rules.size()]);
    }
}
