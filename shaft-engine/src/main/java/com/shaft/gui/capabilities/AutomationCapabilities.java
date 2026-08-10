package com.shaft.gui.capabilities;

import java.util.EnumMap;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable snapshot of the features proven available to one live SHAFT GUI session.
 *
 * <p>A snapshot does not follow later lifecycle changes. Request a new snapshot after navigation
 * infrastructure, driver, context, or connection state changes. Missing entries are always
 * unsupported; callers never need to infer support from backend names or raw capabilities.</p>
 */
public final class AutomationCapabilities {
    private static final String NATIVE_ESCAPE_HATCH =
            "Use getNativeDriver() for a backend-specific extension.";

    private final AutomationBackend backend;
    private final String runtime;
    private final String platform;
    private final String defaultUnsupportedReason;
    private final Map<AutomationFeature, AutomationCapability> capabilities;

    private AutomationCapabilities(Builder builder) {
        backend = builder.backend;
        runtime = normalize(builder.runtime);
        platform = normalize(builder.platform);
        defaultUnsupportedReason = normalize(builder.defaultUnsupportedReason);
        capabilities = Map.copyOf(builder.capabilities);
    }

    /**
     * Starts building a capability snapshot for a backend.
     *
     * @param backend active backend
     * @return a new builder
     */
    public static Builder builder(AutomationBackend backend) {
        return new Builder(backend);
    }

    /**
     * Creates a fail-closed snapshot for an absent or unidentified backend.
     *
     * @param reason reason the backend could not be identified
     * @return an empty capability snapshot
     */
    public static AutomationCapabilities unknown(String reason) {
        return builder(AutomationBackend.UNKNOWN)
                .unsupportedByDefault(reason)
                .build();
    }

    /**
     * @return active backend
     */
    public AutomationBackend backend() {
        return backend;
    }

    /**
     * @return backend runtime or browser description
     */
    public String runtime() {
        return runtime;
    }

    /**
     * @return active platform description
     */
    public String platform() {
        return platform;
    }

    /**
     * Returns the full immutable description for a feature.
     *
     * @param feature feature to inspect
     * @return explicit or fail-closed capability description
     */
    public AutomationCapability capability(AutomationFeature feature) {
        Objects.requireNonNull(feature, "feature");
        return capabilities.getOrDefault(feature, new AutomationCapability(
                feature,
                CapabilitySupport.UNSUPPORTED,
                defaultUnsupportedReason,
                NATIVE_ESCAPE_HATCH));
    }

    /**
     * @param feature feature to inspect
     * @return effective support level
     */
    public CapabilitySupport supportOf(AutomationFeature feature) {
        return capability(feature).support();
    }

    /**
     * @param feature feature to inspect
     * @return {@code true} for native or adapted support
     */
    public boolean supports(AutomationFeature feature) {
        return supportOf(feature) != CapabilitySupport.UNSUPPORTED;
    }

    /**
     * Requires a feature before an operation mutates the active session.
     *
     * @param feature required feature
     * @return this snapshot for fluent preflight checks
     * @throws UnsupportedAutomationFeatureException when the feature is unavailable
     */
    public AutomationCapabilities require(AutomationFeature feature) {
        AutomationCapability capability = capability(feature);
        if (capability.support() == CapabilitySupport.UNSUPPORTED) {
            throw new UnsupportedAutomationFeatureException(this, capability);
        }
        return this;
    }

    private static String normalize(String value) {
        return value == null || value.isBlank() ? "unknown" : value;
    }

    /**
     * Mutable construction helper whose state is defensively copied by {@link #build()}.
     */
    public static final class Builder {
        private final AutomationBackend backend;
        private final EnumMap<AutomationFeature, AutomationCapability> capabilities =
                new EnumMap<>(AutomationFeature.class);
        private String runtime = "unknown";
        private String platform = "unknown";
        private String defaultUnsupportedReason = "The active backend did not declare this feature.";

        private Builder(AutomationBackend backend) {
            this.backend = Objects.requireNonNull(backend, "backend");
        }

        /**
         * @param runtime browser, driver, or automation runtime description
         * @return this builder
         */
        public Builder runtime(String runtime) {
            this.runtime = runtime;
            return this;
        }

        /**
         * @param platform effective platform description
         * @return this builder
         */
        public Builder platform(String platform) {
            this.platform = platform;
            return this;
        }

        /**
         * Sets the reason used for features that receive no explicit entry.
         *
         * @param reason fail-closed reason
         * @return this builder
         */
        public Builder unsupportedByDefault(String reason) {
            this.defaultUnsupportedReason = reason;
            return this;
        }

        /**
         * Declares a feature implemented directly by the backend.
         *
         * @param feature feature to declare
         * @param detail backend implementation detail
         * @return this builder
         */
        public Builder nativeFeature(AutomationFeature feature, String detail) {
            return feature(feature, CapabilitySupport.NATIVE, detail, NATIVE_ESCAPE_HATCH);
        }

        /**
         * Declares a feature whose public semantics SHAFT supplies through an adapter.
         *
         * @param feature feature to declare
         * @param detail adapter implementation detail
         * @return this builder
         */
        public Builder adaptedFeature(AutomationFeature feature, String detail) {
            return feature(feature, CapabilitySupport.ADAPTED, detail, NATIVE_ESCAPE_HATCH);
        }

        /**
         * Declares an unavailable feature with an actionable alternative.
         *
         * @param feature feature to declare
         * @param reason why it is unavailable
         * @param alternative supported alternative or native extension path
         * @return this builder
         */
        public Builder unsupportedFeature(AutomationFeature feature, String reason, String alternative) {
            return feature(feature, CapabilitySupport.UNSUPPORTED, reason, alternative);
        }

        /**
         * Adds or replaces one feature entry. The last entry for a feature wins.
         *
         * @param feature feature to declare
         * @param support effective support level
         * @param detail implementation detail or unsupported reason
         * @param alternative fallback guidance
         * @return this builder
         */
        public Builder feature(
                AutomationFeature feature,
                CapabilitySupport support,
                String detail,
                String alternative) {
            AutomationCapability capability = new AutomationCapability(feature, support, detail, alternative);
            capabilities.put(feature, capability);
            return this;
        }

        /**
         * Creates an immutable defensive copy of the current builder state.
         *
         * @return capability snapshot
         */
        public AutomationCapabilities build() {
            return new AutomationCapabilities(this);
        }
    }
}
