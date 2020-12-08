package nl.njtromp.model;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Region {
    public final String id;
    public final List<LevelReading> readings = new ArrayList<>();

    public Region(String id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return "Region{" +
                "id='" + id + '\'' +
                ", readings=" + readings.stream().filter(l -> l.keep).collect(Collectors.toList()) +
                '}';
    }
}
