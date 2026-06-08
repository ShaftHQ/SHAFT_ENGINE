package com.shaft.gui.internal.video;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.ServiceLoader;

final class DesktopVideoRecordingProviderRegistry {
    private static volatile DesktopVideoRecordingProvider providerOverride;

    private DesktopVideoRecordingProviderRegistry() {
        throw new IllegalStateException("Utility class");
    }

    static Optional<DesktopVideoRecordingProvider> findProvider() {
        if (providerOverride != null) {
            return Optional.of(providerOverride);
        }

        List<DesktopVideoRecordingProvider> providers = ServiceLoader.load(DesktopVideoRecordingProvider.class)
                .stream()
                .map(ServiceLoader.Provider::get)
                .sorted(Comparator.comparing(provider -> provider.getClass().getName()))
                .toList();
        if (providers.size() > 1) {
            String providerNames = providers.stream()
                    .map(provider -> provider.getClass().getName())
                    .sorted()
                    .reduce((left, right) -> left + ", " + right)
                    .orElse("");
            throw new IllegalStateException("Multiple desktop video recording providers were found: " + providerNames);
        }
        return providers.stream().findFirst();
    }

    static void setProviderForTesting(DesktopVideoRecordingProvider provider) {
        providerOverride = provider;
    }

    static void resetProviderForTesting() {
        providerOverride = null;
    }
}
