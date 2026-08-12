package com.shaft.gui.driver;

import java.nio.file.Path;

/** Mobile source, screenshot, and state evidence actions. */
public interface MobileEvidenceActionsContract {
    /**
     * Captures a bounded mobile evidence archive at the caller-selected exact target.
     *
     * @param exactTarget target archive path
     * @return immutable descriptor of the published evidence bundle
     */
    default MobileEvidenceBundle capture(Path exactTarget) {
        throw new UnsupportedOperationException("Mobile evidence capture is not supported by this implementation.");
    }

    MobileActionsContract and();
}
