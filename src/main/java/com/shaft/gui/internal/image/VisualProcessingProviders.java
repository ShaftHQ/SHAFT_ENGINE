package com.shaft.gui.internal.image;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

/**
 * Discovers and caches the visual-processing provider without linking core image classes to optional libraries.
 */
final class VisualProcessingProviders {
    static final String SHAFT_VISUAL_COORDINATE = "io.github.shafthq:shaft-visual";
    private static final String BUILT_IN_PROVIDER = "com.shaft.gui.internal.image.LegacyVisualProcessingProvider";
    private static final String MISSING_PROVIDER_MESSAGE = "Optional visual processing requires the SHAFT visual "
            + "provider. Add `" + SHAFT_VISUAL_COORDINATE + "` to enable OpenCV, Shutterbug, or Applitools visual "
            + "matching.";
    private static volatile Optional<VisualProcessingProvider> cachedProvider;

    private VisualProcessingProviders() {
        throw new IllegalStateException("Utility class");
    }

    static VisualProcessingProvider requireProvider() {
        return getProvider().orElseThrow(() -> new IllegalStateException(MISSING_PROVIDER_MESSAGE));
    }

    static void loadProviderIfPresent() {
        Optional<VisualProcessingProvider> provider = getProvider();
        if (provider.isPresent()) {
            provider.get().initialize();
        } else {
            ReportManager.logDiscrete(MISSING_PROVIDER_MESSAGE);
        }
    }

    static Optional<VisualProcessingProvider> getProvider() {
        Optional<VisualProcessingProvider> provider = cachedProvider;
        if (provider == null) {
            synchronized (VisualProcessingProviders.class) {
                provider = cachedProvider;
                if (provider == null) {
                    provider = discover(Thread.currentThread().getContextClassLoader());
                    cachedProvider = provider;
                }
            }
        }
        return provider;
    }

    static Optional<VisualProcessingProvider> discover(ClassLoader classLoader) {
        ClassLoader discoveryClassLoader = classLoader == null
                ? VisualProcessingProviders.class.getClassLoader()
                : classLoader;
        List<VisualProcessingProvider> providers = new ArrayList<>();
        addServiceProviders(discoveryClassLoader, providers);
        addBuiltInProvider(discoveryClassLoader, providers);
        return selectProvider(providers);
    }

    static Optional<VisualProcessingProvider> selectProvider(Collection<VisualProcessingProvider> providers) {
        return providers.stream()
                .sorted(Comparator.comparingInt(VisualProcessingProvider::priority).reversed()
                        .thenComparing(provider -> provider.getClass().getName()))
                .filter(VisualProcessingProviders::isAvailable)
                .findFirst();
    }

    static void setProviderForTesting(Optional<VisualProcessingProvider> provider) {
        cachedProvider = provider;
    }

    static void resetCacheForTesting() {
        cachedProvider = null;
    }

    private static void addServiceProviders(ClassLoader classLoader, List<VisualProcessingProvider> providers) {
        try {
            ServiceLoader.load(VisualProcessingProvider.class, classLoader).forEach(providers::add);
        } catch (ServiceConfigurationError | LinkageError error) {
            ReportManagerHelper.logDiscrete(error);
        }
    }

    private static void addBuiltInProvider(ClassLoader classLoader, List<VisualProcessingProvider> providers) {
        try {
            Class<?> providerClass = Class.forName(BUILT_IN_PROVIDER, true, classLoader);
            if (VisualProcessingProvider.class.isAssignableFrom(providerClass)) {
                providers.add((VisualProcessingProvider) providerClass.getDeclaredConstructor().newInstance());
            }
        } catch (ReflectiveOperationException | LinkageError error) {
            ReportManagerHelper.logDiscrete(error);
        }
    }

    private static boolean isAvailable(VisualProcessingProvider provider) {
        try {
            return provider.isAvailable();
        } catch (LinkageError | RuntimeException error) {
            ReportManagerHelper.logDiscrete(error);
            return false;
        }
    }
}
