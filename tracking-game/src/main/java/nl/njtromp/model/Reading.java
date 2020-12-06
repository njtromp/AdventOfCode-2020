package nl.njtromp.model;

import java.util.HashMap;
import java.util.Map;

public class Reading {
    public final String date;
    public final int time;
    public final String id;
    public final Map<String, Integer> contaminants = new HashMap<>();

    public Reading(String date, int time, String id) {
        this.date = date;
        this.time = time;
        this.id = id;
    }

    public int getTotal() {
        return contaminants.values().stream().mapToInt(Integer::intValue).sum();
    }

    @Override
    public String toString() {
        return String.format("Reading{\n\tdate:%s,\n\ttime:%d\n\tid:%s\n\tcontaminants:%s\n", date, time, id, contaminants);
    }
}
