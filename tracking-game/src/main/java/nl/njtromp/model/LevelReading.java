package nl.njtromp.model;

import java.util.ArrayList;
import java.util.List;

public class LevelReading {
    public final String id;
    public final String date;
    public final List<Integer> readings = new ArrayList<>();
    public boolean keep;

    public LevelReading(String id, String date) {
        this.id = id;
        this.date = date;
    }

    public int getFloodDanger() {
        int floodDanger = 0;
        int base = readings.get(0);
        for (int i = 0; i < readings.size(); i++) {
            int level = readings.get(i);
            if (level > base) {
                base = level;
            } else {
                floodDanger += base - level;
            }
        }
        return floodDanger;
    }

    @Override
    public String toString() {
        return "LevelReading{" +
                "id='" + id + '\'' +
                ", date='" + date + '\'' +
//                ", readings=" + readings +
                '}';
    }
}
