package com.shaft.gui.internal.healing;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.ServiceLoader;

final class HealingProviderRegistry {
    private static final ThreadLocal<Optional<HealingProvider>> providerOverride = new ThreadLocal<>();
    private static volatile Optional<HealingProvider> cachedProvider;

    private HealingProviderRegistry() {
        throw new IllegalStateException("Utility class");
    }

    static Optional<HealingProvider> findProvider() {
        if (providerOverride.get() != null) {
            return providerOverride.get();
        }
        Optional<HealingProvider> provider = cachedProvider;
        if (provider == null) {
            synchronized (HealingProviderRegistry.class) {
                provider = cachedProvider;
                if (provider == null) {
                    provider = selectProvider(ServiceLoader.load(HealingProvider.class)
                            .stream()
                            .map(ServiceLoader.Provider::get)
                            .toList());
                    cachedProvider = provider;
                }
            }
        }
        return provider;
    }

    static Optional<HealingProvider> selectProvider(List<HealingProvider> providers) {
        List<HealingProvider> sorted = providers.stream()
                .sorted(Comparator.comparing(provider -> provider.getClass().getName()))
                .toList();
        if (sorted.size() > 1) {
            String names = sorted.stream()
                    .map(provider -> provider.getClass().getName())
                    .reduce((left, right) -> left + ", " + right)
                    .orElse("");
            throw new IllegalStateException("Multiple SHAFT Heal providers were found: " + names);
        }
        return sorted.stream().findFirst();
    }

    static void setProviderForTesting(HealingProvider provider) {
        providerOverride.set(Optional.ofNullable(provider));
    }

    static void resetProviderForTesting() {
        providerOverride.remove();
    }
}
