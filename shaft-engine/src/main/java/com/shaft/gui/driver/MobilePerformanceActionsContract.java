package com.shaft.gui.driver;

import java.util.List;

/** Mobile performance-data actions. */
public interface MobilePerformanceActionsContract {
    /** Returns the performance-data types advertised by the live provider. */
    default List<String> supportedTypes() {
        throw unsupported();
    }

    /** Captures one performance-data sample for the requested application and data type. */
    default MobilePerformanceSample sample(String applicationId, String dataType) {
        throw unsupported();
    }

    /** Returns an immutable snapshot of the newest 100 samples captured for this session. */
    default List<MobilePerformanceSample> history() {
        throw unsupported();
    }

    /** Clears captured performance history for this session. */
    default MobilePerformanceActionsContract clear() {
        throw unsupported();
    }

    /** Returns the owning mobile namespace. */
    MobileActionsContract and();

    private static UnsupportedOperationException unsupported() {
        return new UnsupportedOperationException("Mobile performance actions are not supported by this implementation.");
    }
}
