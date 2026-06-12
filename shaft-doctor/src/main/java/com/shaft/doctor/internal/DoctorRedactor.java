package com.shaft.doctor.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.pilot.security.Redactor;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Structured and textual deterministic redaction for Doctor evidence.
 */
public final class DoctorRedactor {
    private static final Set<String> SENSITIVE_FIELDS = Set.of(
            "authorization", "proxy-authorization", "cookie", "set-cookie",
            "password", "passwd", "secret", "token", "access_token", "refresh_token",
            "api-key", "apikey", "api_key", "access-key", "access_key",
            "client_secret", "private_key", "credential", "credentials");
    private static final List<NamedPattern> PATTERNS = List.of(
            new NamedPattern("authorization-header",
                    Pattern.compile("(?im)(authorization|proxy-authorization)\\s*[:=]\\s*[^\\r\\n]+")),
            new NamedPattern("cookie-header",
                    Pattern.compile("(?im)(cookie|set-cookie)\\s*[:=]\\s*[^\\r\\n]+")),
            new NamedPattern("secret-assignment",
                    Pattern.compile("(?i)(password|passwd|secret|api[_-]?key|access[_-]?key|token)"
                            + "\\s*[:=]\\s*([^\\s,;]+)")),
            new NamedPattern("json-secret-field",
                    Pattern.compile("(?i)\"(?:password|passwd|secret|api[_-]?key|access[_-]?key|token"
                            + "|authorization|cookie)\"\\s*:\\s*\"[^\"]*\"")),
            new NamedPattern("bearer-token",
                    Pattern.compile("(?i)bearer\\s+[a-z0-9._~+/=-]+")),
            new NamedPattern("jwt",
                    Pattern.compile("\\beyJ[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\b")),
            new NamedPattern("private-key",
                    Pattern.compile("(?s)-----BEGIN [A-Z ]*PRIVATE KEY-----.*?"
                            + "-----END [A-Z ]*PRIVATE KEY-----")),
            new NamedPattern("aws-access-key",
                    Pattern.compile("\\b(?:AKIA|ASIA)[A-Z0-9]{16}\\b")));

    private final Set<String> appliedRules = new LinkedHashSet<>();
    private final Set<String> removedFields = new LinkedHashSet<>();

    /**
     * Redacts a JSON tree without retaining removed values.
     *
     * @param input input tree
     * @return sanitized copy
     */
    public JsonNode redact(JsonNode input) {
        JsonNode copy = input.deepCopy();
        redactNode(copy);
        return copy;
    }

    /**
     * Redacts textual evidence.
     *
     * @param input input text
     * @return sanitized text
     */
    public String redact(String input) {
        String output = input == null ? "" : input;
        if (looksLikeHtml(output)) {
            output = redactHtml(output);
        }
        for (NamedPattern pattern : PATTERNS) {
            String updated = pattern.pattern().matcher(output).replaceAll(Redactor.REPLACEMENT);
            if (!updated.equals(output)) {
                appliedRules.add(pattern.name());
                output = updated;
            }
        }
        return output;
    }

    /**
     * Returns sorted applied rule names.
     *
     * @return applied rules
     */
    public List<String> appliedRules() {
        return appliedRules.stream().sorted().toList();
    }

    /**
     * Returns sorted removed structured field names.
     *
     * @return removed field names
     */
    public List<String> removedFields() {
        return removedFields.stream().sorted().toList();
    }

    private void redactNode(JsonNode node) {
        if (node instanceof ObjectNode object) {
            redactNamedValue(object);
            List<String> names = new ArrayList<>();
            Iterator<String> iterator = object.fieldNames();
            iterator.forEachRemaining(names::add);
            for (String name : names) {
                JsonNode child = object.get(name);
                if (isSensitive(name)) {
                    object.put(name, Redactor.REPLACEMENT);
                    removedFields.add(name.toLowerCase(Locale.ROOT));
                    appliedRules.add("structured-field");
                } else if (child != null && child.isTextual()) {
                    object.put(name, redact(child.textValue()));
                } else if (child != null) {
                    redactNode(child);
                }
            }
        } else if (node instanceof ArrayNode array) {
            for (int index = 0; index < array.size(); index++) {
                JsonNode child = array.get(index);
                if (child.isTextual()) {
                    array.set(index, array.textNode(redact(child.textValue())));
                } else {
                    redactNode(child);
                }
            }
        }
    }

    private static boolean isSensitive(String name) {
        String normalized = name.toLowerCase(Locale.ROOT);
        if (SENSITIVE_FIELDS.contains(normalized)) {
            return true;
        }
        return normalized.endsWith("password")
                || normalized.endsWith("secret")
                || normalized.endsWith("token")
                || normalized.endsWith("apikey")
                || normalized.endsWith("api_key");
    }

    private void redactNamedValue(ObjectNode object) {
        String semanticName = object.path("name").asText(object.path("key").asText(""));
        if (!semanticName.isBlank() && isSensitive(semanticName)) {
            for (String valueField : List.of("value", "content", "text")) {
                if (object.has(valueField)) {
                    object.put(valueField, Redactor.REPLACEMENT);
                }
            }
            removedFields.add(semanticName.toLowerCase(Locale.ROOT));
            appliedRules.add("structured-name-value");
        }
    }

    private String redactHtml(String input) {
        Document document = Jsoup.parseBodyFragment(input);
        for (Element element : document.select(
                "input[type=password],[autocomplete=current-password],[autocomplete=new-password]")) {
            element.text(Redactor.REPLACEMENT);
            element.attributes().forEach(attribute -> {
                if (!"type".equalsIgnoreCase(attribute.getKey())
                        && !"autocomplete".equalsIgnoreCase(attribute.getKey())) {
                    attribute.setValue(Redactor.REPLACEMENT);
                }
            });
            appliedRules.add("dom-selector");
        }
        for (Element element : document.getAllElements()) {
            List<String> names = element.attributes().asList().stream()
                    .map(org.jsoup.nodes.Attribute::getKey)
                    .toList();
            for (String name : names) {
                if (isSensitive(name)) {
                    element.attr(name, Redactor.REPLACEMENT);
                    removedFields.add(name.toLowerCase(Locale.ROOT));
                    appliedRules.add("dom-attribute");
                }
            }
        }
        return document.body().html();
    }

    private static boolean looksLikeHtml(String value) {
        return value.indexOf('<') >= 0 && value.indexOf('>') > value.indexOf('<');
    }

    private record NamedPattern(String name, Pattern pattern) {
    }
}
