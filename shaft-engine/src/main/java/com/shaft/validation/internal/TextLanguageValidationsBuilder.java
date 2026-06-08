package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;

import java.util.Locale;

/**
 * Fluent builder for validating text language by character-set detection.
 */
public class TextLanguageValidationsBuilder {
    private final NativeValidationsBuilder nativeValidationsBuilder;

    /**
     * Package-private constructor to enforce creation through parent fluent builders.
     *
     * @param nativeValidationsBuilder parent native builder carrying assertion context
     */
    TextLanguageValidationsBuilder(NativeValidationsBuilder nativeValidationsBuilder) {
        this.nativeValidationsBuilder = nativeValidationsBuilder;
    }

    /**
     * Validates that the target text contains characters from the selected language preset.
     *
     * @param language expected language preset
     * @return validations executor for optional custom message and perform
     */
    public ValidationsExecutor is(ValidationEnums.TextLanguage language) {
        return nativeValidationsBuilder.matchesRegex(language.getDetectionRegex());
    }

    /**
     * Validates that the target text does not contain characters from the selected language preset.
     *
     * @param language unexpected language preset
     * @return validations executor for optional custom message and perform
     */
    public ValidationsExecutor isNot(ValidationEnums.TextLanguage language) {
        return nativeValidationsBuilder.doesNotMatchRegex(language.getDetectionRegex());
    }

    /**
     * Validates that the target text matches a supported language code.
     *
     * @param languageCode two-letter language code (e.g., ar, en, es, fr, de)
     * @return validations executor for optional custom message and perform
     * @throws IllegalArgumentException when the language code is unsupported
     */
    public ValidationsExecutor is(String languageCode) {
        return is(parseLanguageCode(languageCode));
    }

    /**
     * Validates that the target text does not match a supported language code.
     *
     * @param languageCode two-letter language code (e.g., ar, en, es, fr, de)
     * @return validations executor for optional custom message and perform
     * @throws IllegalArgumentException when the language code is unsupported
     */
    public ValidationsExecutor isNot(String languageCode) {
        return isNot(parseLanguageCode(languageCode));
    }

    private ValidationEnums.TextLanguage parseLanguageCode(String languageCode) {
        String normalizedLanguageCode = languageCode == null ? "" : languageCode.trim().toLowerCase(Locale.ROOT);
        for (ValidationEnums.TextLanguage language : ValidationEnums.TextLanguage.values()) {
            if (language.getLanguageCode().equals(normalizedLanguageCode)) {
                return language;
            }
        }
        throw new IllegalArgumentException("Unsupported language code \"" + languageCode
                + "\". Supported language codes are: ar, en, es, fr, de.");
    }
}
