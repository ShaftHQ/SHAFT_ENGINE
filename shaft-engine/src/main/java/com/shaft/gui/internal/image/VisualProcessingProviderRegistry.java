package com.shaft.gui.internal.image;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.ServiceLoader;

final class VisualProcessingProviderRegistry {
    static final String MISSING_PROVIDER_MESSAGE = "Optional visual processing requires io.github.shafthq:shaft-visual on the runtime classpath.";
    private static final ThreadLocal<Optional<VisualProcessingProvider>> providerOverride = new ThreadLocal<>();

    private VisualProcessingProviderRegistry() {
        throw new IllegalStateException("Utility class");
    }

    static Optional<VisualProcessingProvider> findProvider() {
        if (providerOverride.get() != null) {
            return providerOverride.get();
        }
        return selectProvider(ServiceLoader.load(VisualProcessingProvider.class)
                .stream()
                .map(ServiceLoader.Provider::get)
                .toList());
    }

    static VisualProcessingProvider requireProvider() {
        return findProvider().orElseThrow(() -> new IllegalStateException(MISSING_PROVIDER_MESSAGE));
    }

    static Optional<VisualProcessingProvider> selectProvider(List<VisualProcessingProvider> providers) {
        List<VisualProcessingProvider> sortedProviders = providers.stream()
                .sorted(Comparator.comparing(provider -> provider.getClass().getName()))
                .toList();
        if (sortedProviders.size() > 1) {
            String providerNames = sortedProviders.stream()
                    .map(provider -> provider.getClass().getName())
                    .reduce((left, right) -> left + ", " + right)
                    .orElse("");
            throw new IllegalStateException("Multiple visual processing providers were found: " + providerNames);
        }
        return sortedProviders.stream().findFirst();
    }

    static void setProviderForTesting(VisualProcessingProvider provider) {
        providerOverride.set(Optional.ofNullable(provider));
    }

    static void resetProviderForTesting() {
        providerOverride.remove();
    }
}
