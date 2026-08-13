package com.shaft.infrastructure;

import java.net.URI;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

/** Release-pinned baseline OCR language models shared by setup and shaft-ocr. */
public final class OcrSetupManifest {
    public static final String TESSDATA_REVISION = "87416418657359cb625c412a48b6e1d6d41c29bd";
    public static final String LICENSE_ID = "Apache-2.0";
    private static final URI BASE_URI = URI.create(
            "https://raw.githubusercontent.com/tesseract-ocr/tessdata_fast/" + TESSDATA_REVISION + "/");
    private static final List<String> BASELINE_LANGUAGES = List.of("eng", "ara");
    private static final Map<String, Model> MODELS = loadModels();

    private OcrSetupManifest() { }

    public static List<SetupAction> actions(SetupMode mode) {
        return actions(mode, List.of());
    }

    public static List<SetupAction> actions(SetupMode mode, List<String> requestedLanguages) {
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        List<String> languages = requestedLanguages.isEmpty() ? BASELINE_LANGUAGES : requestedLanguages;
        return languages.stream().map(language -> {
            Model model = MODELS.get(language);
            if (model == null) throw new IllegalArgumentException("Unsupported OCR language code: " + language);
            return new SetupAction(SetupTarget.OCR_TESSDATA, kind,
                TESSDATA_REVISION + ':' + model.language(), BASE_URI.resolve(model.language() + ".traineddata"),
                model.checksum(), false, Set.of());
        }).toList();
    }

    public static Path modelsDirectory(ShaftCachePaths paths) {
        return paths.cache().resolve("ocr/tessdata-fast-" + TESSDATA_REVISION);
    }

    /** Location used by SHAFT releases before setup adopted the platform-native cache root. */
    public static Path legacyModelsDirectory(Path userHome) {
        return userHome.toAbsolutePath().normalize()
                .resolve(".cache/shaft/ocr/tessdata-fast-" + TESSDATA_REVISION);
    }

    public static List<String> baselineLanguages() {
        return BASELINE_LANGUAGES;
    }

    private static Map<String, Model> loadModels() {
        Map<String, Model> models = new LinkedHashMap<>();
        try (var input = OcrSetupManifest.class.getResourceAsStream("/com/shaft/infrastructure/ocr/tessdata-fast-manifest.tsv")) {
            if (input == null) throw new IllegalStateException("OCR setup manifest is missing.");
            try (var reader = new BufferedReader(new InputStreamReader(input, StandardCharsets.UTF_8))) {
                reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).forEach(line -> {
                    String[] fields = line.split("\\t");
                    if (fields.length != 3 || !fields[0].matches("[a-zA-Z0-9_]+")
                            || !fields[1].matches("[0-9a-f]{64}") || !fields[2].matches("[1-9][0-9]*")) {
                        throw new IllegalStateException("Invalid OCR setup manifest row: " + line);
                    }
                    Model previous = models.putIfAbsent(fields[0],
                            new Model(fields[0], "sha256:" + fields[1], Long.parseLong(fields[2])));
                    if (previous != null) throw new IllegalStateException("Duplicate OCR language: " + fields[0]);
                });
            }
        } catch (java.io.IOException failure) {
            throw new IllegalStateException("Could not read OCR setup manifest.", failure);
        }
        if (!models.keySet().containsAll(BASELINE_LANGUAGES)) {
            throw new IllegalStateException("OCR setup manifest does not contain every baseline language.");
        }
        return Map.copyOf(models);
    }

    private record Model(String language, String checksum, long size) { }
}
