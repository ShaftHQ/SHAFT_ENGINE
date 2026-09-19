package com.shaft.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Gitignored project lexicon of accepted business phrases (issue #5951).
 */
final class DesignLexiconStore {
    static final String RELATIVE = ".shaft/design-lexicon.json";
    private static final Set<String> LOGIN = Set.of("log in", "login", "sign in", "authenticate", "authenticated");

    private DesignLexiconStore() {
    }

    static McpDesignLexicon suggest(Path workspace, String query) {
        List<String> catalog = read(workspace);
        List<String> hits = new ArrayList<>();
        String needle = normalize(query);
        for (String phrase : catalog) {
            if (matches(needle, phrase)) {
                hits.add(phrase);
            }
        }
        return new McpDesignLexicon(
                McpDesignLexicon.CURRENT_SCHEMA_VERSION,
                "ok",
                hits.isEmpty() ? "No catalog matches; a new step is allowed." : "Reuse an existing phrase.",
                hits,
                false);
    }

    static McpDesignLexicon accept(Path workspace, String phrase) throws IOException {
        String clean = phrase == null ? "" : phrase.strip();
        if (clean.isBlank()) {
            return new McpDesignLexicon(
                    McpDesignLexicon.CURRENT_SCHEMA_VERSION, "error", "Phrase is blank.", List.of(), false);
        }
        if (looksLikeLocator(clean)) {
            return new McpDesignLexicon(
                    McpDesignLexicon.CURRENT_SCHEMA_VERSION,
                    "error",
                    "Locator steps are not ubiquitous language.",
                    List.of(),
                    false);
        }
        Set<String> catalog = new LinkedHashSet<>(read(workspace));
        catalog.add(clean);
        Path file = workspace.resolve(RELATIVE);
        Files.createDirectories(file.getParent());
        writeJson(file, catalog);
        return new McpDesignLexicon(
                McpDesignLexicon.CURRENT_SCHEMA_VERSION,
                "ok",
                "Accepted into the local design lexicon.",
                List.copyOf(catalog),
                true);
    }

    private static List<String> read(Path workspace) {
        Path file = workspace.resolve(RELATIVE);
        if (!Files.isRegularFile(file)) {
            return List.of();
        }
        try {
            return readJson(file);
        } catch (IOException ignored) {
            return List.of();
        }
    }

    private static List<String> readJson(Path file) throws IOException {
        String raw = Files.readString(file, StandardCharsets.UTF_8).strip();
        if (raw.isEmpty()) {
            return List.of();
        }
        if (!raw.startsWith("{")) {
            return raw.lines().map(String::strip).filter(line -> !line.isBlank()).toList();
        }
        JsonNode root = new ObjectMapper().readTree(raw);
        JsonNode phrases = root.get("phrases");
        if (phrases == null || !phrases.isArray()) {
            return List.of();
        }
        List<String> values = new ArrayList<>();
        for (JsonNode node : phrases) {
            String phrase = node.asText("").strip();
            if (!phrase.isBlank()) {
                values.add(phrase);
            }
        }
        return values;
    }

    private static void writeJson(Path file, Set<String> catalog) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        ObjectNode root = mapper.createObjectNode();
        ArrayNode phrases = root.putArray("phrases");
        for (String phrase : catalog) {
            phrases.add(phrase);
        }
        mapper.writerWithDefaultPrettyPrinter().writeValue(file.toFile(), root);
    }

    static boolean matches(String query, String phrase) {
        String hay = normalize(phrase);
        if (query.isBlank() || hay.contains(query) || query.contains(hay)) {
            return !query.isBlank();
        }
        boolean loginQuery = LOGIN.stream().anyMatch(query::contains);
        boolean loginPhrase = LOGIN.stream().anyMatch(hay::contains);
        return loginQuery && loginPhrase;
    }

    private static boolean looksLikeLocator(String phrase) {
        String lower = phrase.toLowerCase(Locale.ROOT);
        return lower.contains("xpath") || lower.contains("css selector") || lower.contains("element_click");
    }

    private static String normalize(String value) {
        return value == null ? "" : value.toLowerCase(Locale.ROOT).strip();
    }
}
