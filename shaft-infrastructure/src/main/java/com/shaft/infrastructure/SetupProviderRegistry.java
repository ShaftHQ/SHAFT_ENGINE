package com.shaft.infrastructure;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/** Immutable profile-to-provider registry with deterministic duplicate and missing-provider failures. */
public final class SetupProviderRegistry {
    private final Map<SetupProfile, SetupProvider> providers;

    public SetupProviderRegistry(List<? extends SetupProvider> providers) {
        Objects.requireNonNull(providers, "providers");
        LinkedHashMap<SetupProfile, SetupProvider> indexed = new LinkedHashMap<>();
        for (SetupProvider provider : providers) {
            SetupProvider value = Objects.requireNonNull(provider, "provider");
            SetupProvider previous = indexed.putIfAbsent(
                    Objects.requireNonNull(value.profile(), "provider.profile"), value);
            if (previous != null) throw new IllegalArgumentException("Duplicate setup provider: " + value.profile());
        }
        this.providers = Map.copyOf(indexed);
    }

    public SetupProvider require(SetupProfile profile) {
        SetupProvider provider = providers.get(Objects.requireNonNull(profile, "profile"));
        if (provider == null) throw new IllegalArgumentException("No setup provider is available for profile "
                + profile + '.');
        return provider;
    }

    public List<SetupProfile> profiles() {
        return providers.keySet().stream().sorted().toList();
    }
}
