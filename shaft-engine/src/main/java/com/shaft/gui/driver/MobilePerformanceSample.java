package com.shaft.gui.driver;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/** One immutable, tabular mobile performance sample returned by Appium. */
public record MobilePerformanceSample(Instant capturedAt, String applicationId, String dataType,
                                      List<String> columns, List<List<Object>> rows) {
    private static final Set<Class<?>> IMMUTABLE_NUMBER_TYPES = Set.of(
            Byte.class, Short.class, Integer.class, Long.class, Float.class, Double.class,
            BigInteger.class, BigDecimal.class);

    public MobilePerformanceSample {
        capturedAt = Objects.requireNonNull(capturedAt, "capture time");
        applicationId = required(applicationId, "application id");
        dataType = required(dataType, "data type");
        columns = copyColumns(columns);
        rows = copyRows(rows, columns.size());
    }

    @Override
    public List<String> columns() {
        return List.copyOf(columns);
    }

    @Override
    public List<List<Object>> rows() {
        return rows;
    }

    private static List<String> copyColumns(List<String> source) {
        Objects.requireNonNull(source, "columns");
        if (source.isEmpty()) {
            throw new IllegalArgumentException("performance data must contain at least one column");
        }
        List<String> copy = source.stream().map(column -> required(column, "column name")).toList();
        if (new HashSet<>(copy).size() != copy.size()) {
            throw new IllegalArgumentException("performance data column names must be unique");
        }
        return copy;
    }

    private static List<List<Object>> copyRows(List<List<Object>> source, int width) {
        Objects.requireNonNull(source, "rows");
        List<List<Object>> copy = new ArrayList<>(source.size());
        for (List<Object> row : source) {
            Objects.requireNonNull(row, "performance data row");
            if (row.size() != width) {
                throw new IllegalArgumentException("performance data row width must match its columns");
            }
            ArrayList<Object> rowCopy = new ArrayList<>(row.size());
            for (Object value : row) {
                if (!isImmutableScalar(value)) {
                    throw new IllegalArgumentException("performance data values must be immutable JSON scalars");
                }
                rowCopy.add(value);
            }
            copy.add(Collections.unmodifiableList(rowCopy));
        }
        return Collections.unmodifiableList(copy);
    }

    private static boolean isImmutableScalar(Object value) {
        return value == null || value instanceof String || value instanceof Boolean
                || IMMUTABLE_NUMBER_TYPES.contains(value.getClass());
    }

    private static String required(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
        return value;
    }
}
