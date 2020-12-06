package nl.njtromp.model;

import java.util.ArrayList;
import java.util.List;

public class LevelReading {
    public final String id;
    public final String date;
    public final List<Integer> readings = new ArrayList<>();

    public LevelReading(String id, String date) {
        this.id = id;
        this.date = date;
    }

    @Override
    public String toString() {
        return "LevelReading{" +
                "id='" + id + '\'' +
                ", date='" + date + '\'' +
                ", readings=" + readings +
                '}';
    }
}
