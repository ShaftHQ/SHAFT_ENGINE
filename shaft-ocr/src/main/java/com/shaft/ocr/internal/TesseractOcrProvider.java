package com.shaft.ocr.internal;

import com.shaft.gui.internal.ocr.OcrDocumentPageAnalysis;
import com.shaft.gui.internal.ocr.OcrProcessingProvider;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrResult;

import java.net.URI;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.LinkedHashMap;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.infrastructure.OcrSetupManifest;
import com.shaft.infrastructure.ShaftCachePaths;

/** Self-contained JavaCPP-backed local Tesseract provider. */
public final class TesseractOcrProvider implements OcrProcessingProvider {
    static final String TESSDATA_REVISION = OcrSetupManifest.TESSDATA_REVISION;
    private static final URI DEFAULT_MODEL_BASE = URI.create(
            "https://raw.githubusercontent.com/tesseract-ocr/tessdata_fast/" + TESSDATA_REVISION + "/");
    private static final Map<String, String> PINNED_GIT_BLOB_IDS = loadModelManifest();

    private final TessdataModelManager models;
    private final TesseractBackend backend;

    public TesseractOcrProvider() {
        this(defaultModelManager(), new JavaCppTesseractBackend());
    }

    TesseractOcrProvider(TessdataModelManager models, TesseractBackend backend) {
        this.models = Objects.requireNonNull(models, "models");
        this.backend = Objects.requireNonNull(backend, "backend");
    }

    @Override
    public OcrResult recognize(byte[] image, OcrOptions options) {
        Objects.requireNonNull(options, "options");
        List<String> languages = OcrLanguageRegistry.resolve(options.languages());
        Path tessdata = models.ensureAvailable(languages);
        return backend.recognize(image, tessdata, String.join("+", languages), options);
    }

    @Override
    public OcrDocumentPageAnalysis analyzeDocumentPage(byte[] image, OcrOptions options,
                                                       boolean detectOrientation, boolean deskew) {
        Objects.requireNonNull(options, "options");
        List<String> languages = OcrLanguageRegistry.resolve(options.languages());
        List<String> requiredModels = new java.util.ArrayList<>(languages);
        if (detectOrientation) {
            requiredModels.add("osd");
        }
        Path tessdata = models.ensureAvailable(requiredModels);
        return backend.analyzeDocumentPage(image, tessdata, String.join("+", languages), options,
                detectOrientation, deskew);
    }

    @Override
    public String name() {
        return "tesseract-local";
    }

    @Override
    public int priority() {
        return 100;
    }

    private static TessdataModelManager defaultModelManager() {
        Object configured = typedProperties();
        String configuredCache = configured == null
                ? effectiveProperty("shaft.ocr.cacheDirectory", "").trim()
                : invokeString(configured, "cacheDirectory").trim();
        Path setupCache = OcrSetupManifest.modelsDirectory(ShaftCachePaths.current());
        Path cache = configuredCache.isEmpty() ? setupCache : absoluteConfiguredCache(configuredCache);
        Path fallback = configuredCache.isEmpty() ? null : setupCache;
        boolean downloadsEnabled = configured == null
                ? Boolean.parseBoolean(effectiveProperty("shaft.ocr.downloadEnabled", "true"))
                : invokeBoolean(configured, "downloadEnabled");
        return new TessdataModelManager(cache, fallback, DEFAULT_MODEL_BASE, downloadsEnabled, PINNED_GIT_BLOB_IDS,
                TessdataModelManager.IntegrityAlgorithm.GIT_BLOB_SHA1);
    }

    private static Path absoluteConfiguredCache(String configuredCache) {
        Path path = Path.of(configuredCache).normalize();
        if (!path.isAbsolute()) {
            throw new IllegalArgumentException("shaft.ocr.cacheDirectory must be absolute.");
        }
        return path;
    }

    private static Object typedProperties() {
        try {
            return Class.forName("com.shaft.properties.internal.Properties").getField("ocr").get(null);
        } catch (ReflectiveOperationException | LinkageError ignored) {
            // A module-local development build may resolve a stale same-version engine from ~/.m2.
            return null;
        }
    }

    private static String invokeString(Object configured, String method) {
        try {
            return (String) configured.getClass().getMethod(method).invoke(configured);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Could not read SHAFT OCR configuration '" + method + "'.", exception);
        }
    }

    private static boolean invokeBoolean(Object configured, String method) {
        try {
            return (boolean) configured.getClass().getMethod(method).invoke(configured);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Could not read SHAFT OCR configuration '" + method + "'.", exception);
        }
    }

    private static String effectiveProperty(String key, String defaultValue) {
        String value = ThreadLocalPropertiesManager.getProperty(key);
        return value == null ? defaultValue : value;
    }

    private static Map<String, String> loadModelManifest() {
        Map<String, String> models = new LinkedHashMap<>();
        try (var stream = TesseractOcrProvider.class.getResourceAsStream("tessdata-fast-manifest.tsv")) {
            if (stream == null) {
                throw new IllegalStateException("SHAFT OCR tessdata integrity manifest is missing.");
            }
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))) {
                reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).forEach(line -> {
                    String[] fields = line.split("\\t");
                    if (fields.length != 3 || !fields[0].endsWith(".traineddata") || fields[1].length() != 40) {
                        throw new IllegalStateException("Invalid SHAFT OCR tessdata manifest entry: " + line);
                    }
                    models.put(fields[0].substring(0, fields[0].length() - ".traineddata".length()), fields[1]);
                });
            }
        } catch (java.io.IOException exception) {
            throw new IllegalStateException("Could not read SHAFT OCR tessdata integrity manifest.", exception);
        }
        return Map.copyOf(models);
    }
}
