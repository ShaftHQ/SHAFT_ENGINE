package com.shaft.intellij.properties;

import com.shaft.intellij.secrets.ShaftSecretsGuardrails;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Schema-aware view of effective SHAFT properties for the profile card (W2.4).
 */
public final class ShaftPropertiesProfileCard {
    public record EffectiveProperty(@NotNull String key, @NotNull String displayValue,
                                    @NotNull String source, boolean secret, boolean deprecated) {
    }

    private ShaftPropertiesProfileCard() {
        throw new IllegalStateException("Utility class");
    }

    public static @NotNull Map<String, EffectiveProperty> loadEffective(@NotNull Path projectRoot) throws IOException {
        Map<String, EffectiveProperty> effective = new LinkedHashMap<>();
        Path custom = projectRoot.resolve("src/main/resources/custom.properties");
        if (Files.isRegularFile(custom)) {
            merge(effective, custom, "custom.properties", false);
        }
        Path secrets = projectRoot.resolve("src/main/resources/secrets.properties");
        if (Files.isRegularFile(secrets)) {
            merge(effective, secrets, "secrets.properties", true);
        }
        for (ShaftProperty known : ShaftPropertyCatalog.all()) {
            effective.computeIfAbsent(known.key(), key -> new EffectiveProperty(
                    key, "(project default)", "schema", ShaftSecretsGuardrails.looksLikeSecretKey(key), isDeprecated(known)));
        }
        return Map.copyOf(effective);
    }

    private static boolean isDeprecated(@NotNull ShaftProperty property) {
        String description = property.description() == null ? "" : property.description().toLowerCase();
        return description.contains("deprecated");
    }

    private static void merge(Map<String, EffectiveProperty> into, Path file, String source, boolean forceSecret)
            throws IOException {
        Properties properties = new Properties();
        try (var reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
            properties.load(reader);
        }
        for (String name : properties.stringPropertyNames()) {
            String raw = properties.getProperty(name, "");
            boolean secret = forceSecret || ShaftSecretsGuardrails.looksLikeSecretKey(name);
            String display = secret ? ShaftSecretsGuardrails.maskValue(raw) : raw;
            ShaftProperty catalog = ShaftPropertyCatalog.find(name);
            boolean deprecated = catalog != null && isDeprecated(catalog);
            into.put(name, new EffectiveProperty(name, display, source, secret, deprecated));
        }
    }
}
