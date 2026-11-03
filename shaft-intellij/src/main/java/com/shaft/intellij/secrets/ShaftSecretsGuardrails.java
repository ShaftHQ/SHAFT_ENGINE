package com.shaft.intellij.secrets;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Secrets file detection, gitignore ensure, masking, and .example generation (W2.5 / field F4).
 */
public final class ShaftSecretsGuardrails {
    private static final List<String> SECRET_FILE_NAMES = List.of(
            "secrets.properties",
            "secret.properties",
            "credentials.properties");
    private static final Pattern SECRET_KEY = Pattern.compile(
            "(?i).*(password|secret|token|access[._-]?key|api[._-]?key|private[._-]?key).*");

    private ShaftSecretsGuardrails() {
        throw new IllegalStateException("Utility class");
    }

    public static @NotNull List<Path> findSecretsFiles(@NotNull Path projectRoot) throws IOException {
        List<Path> found = new ArrayList<>();
        Path resources = projectRoot.resolve("src/main/resources");
        if (Files.isDirectory(resources)) {
            for (String name : SECRET_FILE_NAMES) {
                Path candidate = resources.resolve(name);
                if (Files.isRegularFile(candidate)) {
                    found.add(candidate);
                }
            }
        }
        for (String name : SECRET_FILE_NAMES) {
            Path candidate = projectRoot.resolve(name);
            if (Files.isRegularFile(candidate)) {
                found.add(candidate);
            }
        }
        return List.copyOf(found);
    }

    public static boolean gitignoreMentionsSecrets(@NotNull Path projectRoot) throws IOException {
        Path gitignore = projectRoot.resolve(".gitignore");
        if (!Files.isRegularFile(gitignore)) {
            return false;
        }
        String text = Files.readString(gitignore, StandardCharsets.UTF_8).toLowerCase(Locale.ROOT);
        return text.contains("secrets.properties") || text.contains("secret.properties");
    }

    public static boolean ensureGitignoreIgnoresSecrets(@NotNull Path projectRoot) throws IOException {
        if (gitignoreMentionsSecrets(projectRoot)) {
            return false;
        }
        Path gitignore = projectRoot.resolve(".gitignore");
        String addition = (Files.isRegularFile(gitignore) ? "\n" : "")
                + "# SHAFT secrets (added by IntelliJ plugin)\n"
                + "secrets.properties\n"
                + "**/secrets.properties\n";
        if (Files.isRegularFile(gitignore)) {
            Files.writeString(gitignore, Files.readString(gitignore, StandardCharsets.UTF_8) + addition,
                    StandardCharsets.UTF_8);
        } else {
            Files.writeString(gitignore, addition, StandardCharsets.UTF_8);
        }
        return true;
    }

    public static @NotNull Path writeExampleFile(@NotNull Path projectRoot) throws IOException {
        Path resources = projectRoot.resolve("src/main/resources");
        Files.createDirectories(resources);
        Path example = resources.resolve("secrets.properties.example");
        if (!Files.isRegularFile(example)) {
            Files.writeString(example,
                    "# Copy to secrets.properties (gitignored) and fill values.\n"
                            + "# cloud.username=\n"
                            + "# cloud.accessKey=\n",
                    StandardCharsets.UTF_8);
        }
        return example;
    }

    public static boolean looksLikeSecretKey(@NotNull String key) {
        return SECRET_KEY.matcher(key.trim()).matches();
    }

    public static @NotNull String maskValue(@NotNull String raw) {
        String trimmed = raw.trim();
        if (trimmed.isEmpty()) {
            return "";
        }
        if (trimmed.length() <= 4) {
            return "****";
        }
        return trimmed.substring(0, 2) + "****" + trimmed.substring(trimmed.length() - 2);
    }

    public static @NotNull String maskPropertiesLine(@NotNull String line) {
        int eq = line.indexOf('=');
        if (eq <= 0) {
            return line;
        }
        String key = line.substring(0, eq).trim();
        if (!looksLikeSecretKey(key)) {
            return line;
        }
        return key + "=" + maskValue(line.substring(eq + 1));
    }
}
