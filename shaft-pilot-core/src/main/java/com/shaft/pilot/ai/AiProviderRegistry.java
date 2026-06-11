package com.shaft.pilot.ai;

import com.shaft.pilot.config.PilotConfiguration;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.ServiceLoader;

/**
 * Resolves explicit current-thread providers before {@link ServiceLoader} providers,
 * then falls back to the disabled provider.
 */
public final class AiProviderRegistry {
    private static final DisabledAiProvider DISABLED_PROVIDER = new DisabledAiProvider();
    private static final ThreadLocal<AiProvider> EXPLICIT_PROVIDER = new ThreadLocal<>();

    /**
     * Registers a provider for the current thread.
     *
     * <p>This is intended for tests and controlled embedding. Call
     * {@link #clearForCurrentThread()} at the lifecycle boundary.</p>
     *
     * @param provider provider override
     */
    public void registerForCurrentThread(AiProvider provider) {
        if (provider == null) {
            EXPLICIT_PROVIDER.remove();
        } else {
            EXPLICIT_PROVIDER.set(provider);
        }
    }

    /**
     * Clears the explicit provider for the current thread.
     */
    public void clearForCurrentThread() {
        EXPLICIT_PROVIDER.remove();
    }

    /**
     * Resolves the configured provider using explicit, service, then disabled precedence.
     *
     * @param configuration effective configuration
     * @return resolved provider
     */
    public AiProvider resolve(PilotConfiguration configuration) {
        if (!configuration.enabled() || "none".equals(configuration.provider())) {
            return DISABLED_PROVIDER;
        }
        AiProvider explicit = EXPLICIT_PROVIDER.get();
        if (explicit != null) {
            return explicit;
        }
        List<AiProvider> matches = serviceProviders().stream()
                .filter(provider -> normalize(provider.id()).equals(configuration.provider()))
                .toList();
        if (matches.size() > 1) {
            String names = matches.stream().map(provider -> provider.getClass().getName())
                    .sorted().reduce((left, right) -> left + ", " + right).orElse("");
            throw new IllegalStateException("Multiple AI providers were found for "
                    + configuration.provider() + ": " + names);
        }
        return matches.stream().findFirst().orElse(DISABLED_PROVIDER);
    }

    /**
     * Returns deterministically ordered service providers.
     *
     * @return discovered providers
     */
    public List<AiProvider> serviceProviders() {
        return ServiceLoader.load(AiProvider.class).stream()
                .map(ServiceLoader.Provider::get)
                .sorted(Comparator.comparing(provider -> provider.getClass().getName()))
                .toList();
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }
}
