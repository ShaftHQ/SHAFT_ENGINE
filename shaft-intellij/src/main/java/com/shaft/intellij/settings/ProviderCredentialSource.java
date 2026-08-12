package com.shaft.intellij.settings;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.function.Function;

/** Provider-standard environment-variable registry. Values never enter UI labels or persistence. */
public final class ProviderCredentialSource {
    private static final Map<String, List<String>> VARIABLES = Map.of(
            "openai", List.of("OPENAI_API_KEY"),
            "anthropic", List.of("ANTHROPIC_API_KEY"),
            "gemini", List.of("GOOGLE_API_KEY", "GEMINI_API_KEY"),
            "github", List.of("GH_TOKEN", "GITHUB_TOKEN"),
            "ollama", List.of("OLLAMA_API_KEY"),
            "lmstudio", List.of("LMSTUDIO_API_KEY"));

    private ProviderCredentialSource() {
    }

    public static List<String> variables(String provider) {
        return VARIABLES.getOrDefault(normalize(provider), List.of());
    }

    public static Optional<Source> detect(String provider) {
        return detect(provider, System::getenv);
    }

    static Optional<Source> detect(String provider, Map<String, String> environment) {
        return detect(provider, environment::get);
    }

    public static Optional<Source> detect(String provider, Function<String, String> lookup) {
        return present(provider, lookup).stream().findFirst();
    }

    public static List<Source> present(String provider, Function<String, String> lookup) {
        return variables(provider).stream()
                .filter(name -> lookup.apply(name) != null && !lookup.apply(name).isBlank())
                .map(Source::new)
                .collect(Collectors.toUnmodifiableList());
    }

    public static boolean supports(String provider, String variableName) {
        return variableName != null && variables(provider).contains(variableName);
    }

    private static String normalize(String provider) {
        return provider == null ? "" : provider.trim().toLowerCase(Locale.ROOT);
    }

    public record Source(String variableName) {
        public String label() {
            return "Use configured " + variableName;
        }
    }
}
