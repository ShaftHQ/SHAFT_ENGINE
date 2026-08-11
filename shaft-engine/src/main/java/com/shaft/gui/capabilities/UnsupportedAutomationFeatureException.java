package com.shaft.gui.capabilities;

/**
 * Signals that an operation is not available for the active automation backend.
 */
public class UnsupportedAutomationFeatureException extends UnsupportedOperationException {
    private final AutomationFeature feature;
    private final AutomationBackend backend;

    /**
     * Creates an actionable unsupported-feature failure.
     *
     * @param capabilities active capability snapshot
     * @param capability unsupported feature description
     */
    public UnsupportedAutomationFeatureException(
            AutomationCapabilities capabilities,
            AutomationCapability capability) {
        super("Automation feature " + capability.feature()
                + " is not supported by " + capabilities.backend()
                + " [runtime=" + capabilities.runtime()
                + ", platform=" + capabilities.platform() + "]. "
                + capability.detail() + " Alternative: " + capability.alternative());
        this.feature = capability.feature();
        this.backend = capabilities.backend();
    }

    /**
     * @return the unsupported feature
     */
    public AutomationFeature feature() {
        return feature;
    }

    /**
     * @return the active backend
     */
    public AutomationBackend backend() {
        return backend;
    }
}
