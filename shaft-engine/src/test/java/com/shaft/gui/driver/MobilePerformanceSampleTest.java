package com.shaft.gui.driver;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

public class MobilePerformanceSampleTest {
    @Test
    public void sampleShouldDeepCopyInputsAndExposeAnImmutableTable() {
        List<String> columns = new ArrayList<>(List.of("user", "system"));
        List<Object> mutableRow = new ArrayList<>(List.of(12, new BigDecimal("3.5")));
        List<List<Object>> rows = new ArrayList<>();
        rows.add(mutableRow);

        MobilePerformanceSample sample = new MobilePerformanceSample(
                Instant.EPOCH, "com.example.app", "cpuinfo", columns, rows);
        columns.set(0, "changed");
        mutableRow.set(0, 99);
        rows.clear();

        Assert.assertEquals(sample.columns(), List.of("user", "system"));
        Assert.assertEquals(sample.rows(), List.of(List.of(12, new BigDecimal("3.5"))));
        Assert.expectThrows(UnsupportedOperationException.class, () -> sample.columns().add("total"));
        Assert.expectThrows(UnsupportedOperationException.class, () -> sample.rows().add(List.of(1, 2)));
        Assert.expectThrows(UnsupportedOperationException.class, () -> sample.rows().getFirst().set(0, 4));
    }

    @Test
    public void sampleShouldPreserveNullJsonScalars() {
        MobilePerformanceSample sample = new MobilePerformanceSample(
                Instant.EPOCH, "com.example.app", "memoryinfo", List.of("value"),
                java.util.Collections.singletonList(java.util.Collections.singletonList(null)));

        Assert.assertNull(sample.rows().getFirst().getFirst());
    }

    @Test
    public void sampleShouldRejectMalformedTables() {
        Instant now = Instant.EPOCH;
        Assert.expectThrows(NullPointerException.class,
                () -> new MobilePerformanceSample(null, "app", "cpu", List.of("value"), List.of()));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobilePerformanceSample(now, " ", "cpu", List.of("value"), List.of()));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobilePerformanceSample(now, "app", " ", List.of("value"), List.of()));
        Assert.expectThrows(NullPointerException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", null, List.of()));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", List.of(), List.of()));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", List.of("value", "value"), List.of()));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", List.of(" "), List.of()));
        Assert.expectThrows(NullPointerException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", List.of("value"), null));
        Assert.expectThrows(NullPointerException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", List.of("value"),
                        java.util.Collections.singletonList(null)));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", List.of("value"), List.of(List.of(1, 2))));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobilePerformanceSample(now, "app", "cpu", List.of("value"),
                        List.of(List.of(new StringBuilder("mutable")))));
    }
}
