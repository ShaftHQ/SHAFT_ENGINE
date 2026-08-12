package com.shaft.gui.internal.ocr;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.ServiceLoader;

final class OcrProcessingProviderRegistry {
    private static final ThreadLocal<List<OcrProcessingProvider>> providersOverride = new ThreadLocal<>();

    private OcrProcessingProviderRegistry() {
    }

    static Optional<OcrProcessingProvider> findProvider() {
        List<OcrProcessingProvider> overridden = providersOverride.get();
        if (overridden != null) {
            return selectProvider(overridden);
        }
        return selectProvider(ServiceLoader.load(OcrProcessingProvider.class).stream()
                .map(ServiceLoader.Provider::get)
                .toList());
    }

    static OcrProcessingProvider requireProvider() {
        return findProvider().orElseThrow(() -> new IllegalStateException(
                "No SHAFT OCR provider is installed. Add io.github.shafthq:shaft-ocr to the test runtime classpath."));
    }

    static Optional<OcrProcessingProvider> selectProvider(List<OcrProcessingProvider> providers) {
        if (providers == null || providers.isEmpty()) {
            return Optional.empty();
        }
        List<OcrProcessingProvider> ordered = providers.stream()
                .sorted(Comparator.comparingInt(OcrProcessingProvider::priority).reversed()
                        .thenComparing(OcrProcessingProvider::name))
                .toList();
        OcrProcessingProvider selected = ordered.getFirst();
        List<OcrProcessingProvider> tied = ordered.stream()
                .filter(provider -> provider.priority() == selected.priority())
                .toList();
        if (tied.size() > 1) {
            throw new IllegalStateException("Multiple SHAFT OCR providers have priority " + selected.priority()
                    + ": " + tied.stream().map(OcrProcessingProvider::name).toList());
        }
        return Optional.of(selected);
    }

    static void setProvidersForTesting(List<OcrProcessingProvider> providers) {
        providersOverride.set(List.copyOf(providers));
    }

    static void clearProviderForTesting() {
        providersOverride.remove();
    }
}
