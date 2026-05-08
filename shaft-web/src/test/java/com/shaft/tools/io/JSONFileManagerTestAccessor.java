package com.shaft.tools.io;

import java.io.FileReader;
import java.lang.reflect.Field;

/**
 * Test accessor for package-private and private state in {@link JSONFileManager}.
 * Used exclusively by unit tests to verify ThreadLocal lifecycle behavior.
 */
public final class JSONFileManagerTestAccessor {

    private JSONFileManagerTestAccessor() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the current value of the static {@code reader} {@link ThreadLocal}
     * in {@link JSONFileManager} for the calling thread.
     *
     * @return the {@link FileReader} bound to the calling thread, or {@code null} if none
     */
    @SuppressWarnings("unchecked")
    public static FileReader getReader() {
        try {
            Field field = JSONFileManager.class.getDeclaredField("reader");
            field.setAccessible(true);
            ThreadLocal<FileReader> tl = (ThreadLocal<FileReader>) field.get(null);
            return tl.get();
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException("Failed to read JSONFileManager.reader via reflection", e);
        }
    }
}
