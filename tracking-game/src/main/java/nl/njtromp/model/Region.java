package nl.njtromp.model;

import java.util.ArrayList;
import java.util.List;

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
                ", readings=" + readings +
                '}';
    }
}
