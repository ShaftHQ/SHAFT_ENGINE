package com.shaft.ocr.internal;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;

final class OcrLanguageRegistry {
    private static final Map<String, String> NAMES_TO_CODES = buildNamesToCodes();
    private static final List<String> DEFAULT_LANGUAGES = List.of("eng", "ara");

    private OcrLanguageRegistry() {
    }

    static List<String> resolve(List<String> configuredLanguages) {
        if (configuredLanguages == null || configuredLanguages.isEmpty()) {
            return DEFAULT_LANGUAGES;
        }
        LinkedHashSet<String> resolved = new LinkedHashSet<>();
        for (String configuredLanguage : configuredLanguages) {
            if (configuredLanguage == null || configuredLanguage.isBlank()) {
                throw new IllegalArgumentException("OCR language names cannot be null or blank.");
            }
            String value = configuredLanguage.trim();
            String code = value.length() == 3
                    ? value.toLowerCase(Locale.ROOT)
                    : NAMES_TO_CODES.get(value.toLowerCase(Locale.ROOT));
            if (code == null) {
                throw new IllegalArgumentException("Unsupported OCR language '" + value
                        + "'. Use a Tesseract three-letter language code or a supported name such as English or Arabic.");
            }
            resolved.add(code);
        }
        return List.copyOf(resolved);
    }

    private static Map<String, String> buildNamesToCodes() {
        Map<String, String> languages = new LinkedHashMap<>();
        List<Locale> locales = new ArrayList<>(List.of(Locale.getAvailableLocales()));
        locales.sort(Comparator.comparing(locale -> locale.getDisplayLanguage(Locale.ENGLISH)));
        for (Locale locale : locales) {
            String displayName = locale.getDisplayLanguage(Locale.ENGLISH);
            if (displayName.isBlank()) {
                continue;
            }
            try {
                String code = locale.getISO3Language();
                if (!code.isBlank()) {
                    languages.putIfAbsent(displayName.toLowerCase(Locale.ROOT), code);
                }
            } catch (java.util.MissingResourceException ignored) {
                // Locale has no ISO-639-2 mapping and cannot name a Tesseract model.
            }
        }
        languages.put("arabic", "ara");
        languages.put("english", "eng");
        languages.put("french", "fra");
        languages.put("german", "deu");
        languages.put("chinese", "chi_sim");
        languages.put("simplified chinese", "chi_sim");
        languages.put("chinese (simplified)", "chi_sim");
        languages.put("traditional chinese", "chi_tra");
        languages.put("chinese (traditional)", "chi_tra");
        return Map.copyOf(languages);
    }
}
