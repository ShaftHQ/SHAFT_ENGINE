package com.shaft.doctor.internal;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.pilot.security.Redactor;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;
import javax.imageio.ImageIO;

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
            object.forEachEntry((name, ignored) -> names.add(name));
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

    /**
     * Determines if HTML page snapshot indicates sensitive fields requiring screenshot masking.
     * Returns empty list if no sensitive fields found, or a special marker indicating whole-image masking.
     *
     * Since jsoup parses static HTML (not rendered layout), real pixel coordinates are not derivable.
     * This method adopts a conservative approach: when sensitive input fields are detected,
     * it returns a marker indicating full-image masking rather than fake precise regions.
     *
     * @param htmlContent HTML page snapshot content
     * @return empty list if no sensitive fields, or list with single marker element for whole-image masking
     */
    public List<String> extractSensitiveRegions(String htmlContent) {
        if (htmlContent == null || htmlContent.isBlank()) {
            return List.of();
        }
        Document document = Jsoup.parseBodyFragment(htmlContent);
        boolean hasSensitiveFields = false;
        for (Element element : document.select(
                "input[type=password],[autocomplete=current-password],[autocomplete=new-password]")) {
            hasSensitiveFields = true;
            break;
        }
        if (!hasSensitiveFields) {
            for (Element element : document.getAllElements()) {
                for (org.jsoup.nodes.Attribute attribute : element.attributes().asList()) {
                    if (isSensitive(attribute.getKey())) {
                        hasSensitiveFields = true;
                        break;
                    }
                }
                if (hasSensitiveFields) {
                    break;
                }
            }
        }
        return hasSensitiveFields ? List.of("WHOLE_IMAGE") : List.of();
    }

    /**
     * Applies full-image opaque masking to screenshot bytes when sensitive regions are detected.
     * Only masks if regions list contains the whole-image marker.
     *
     * @param screenshotBytes original screenshot image bytes
     * @param regions masking decision (empty = no masking, contains "WHOLE_IMAGE" = mask entire image)
     * @return masked image bytes, or original bytes if no masking needed
     * @throws IOException if image reading/writing fails
     */
    public byte[] redactScreenshot(byte[] screenshotBytes, List<String> regions) throws IOException {
        if (screenshotBytes == null || screenshotBytes.length == 0) {
            return screenshotBytes;
        }
        if (regions == null || regions.isEmpty() || !regions.contains("WHOLE_IMAGE")) {
            return screenshotBytes;
        }
        try {
            BufferedImage image = ImageIO.read(new ByteArrayInputStream(screenshotBytes));
            if (image == null) {
                return screenshotBytes;
            }
            int width = image.getWidth();
            int height = image.getHeight();
            BufferedImage masked = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            int opaqueBlack = 0xFF000000;
            for (int y = 0; y < height; y++) {
                for (int x = 0; x < width; x++) {
                    masked.setRGB(x, y, opaqueBlack);
                }
            }
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            String extension = guessImageFormat(screenshotBytes);
            if (extension == null || extension.isEmpty()) {
                extension = "png";
            }
            ImageIO.write(masked, extension, outputStream);
            return outputStream.toByteArray();
        } catch (IOException exception) {
            return screenshotBytes;
        }
    }

    private static String guessImageFormat(byte[] bytes) {
        if (bytes == null || bytes.length < 4) {
            return "png";
        }
        if (bytes[0] == (byte) 0xFF && bytes[1] == (byte) 0xD8 && bytes[2] == (byte) 0xFF) {
            return "jpg";
        }
        if (bytes[0] == (byte) 0x89 && bytes[1] == 0x50 && bytes[2] == 0x4E && bytes[3] == 0x47) {
            return "png";
        }
        if (bytes[0] == 0x47 && bytes[1] == 0x49 && bytes[2] == 0x46) {
            return "gif";
        }
        if (bytes[0] == 0x52 && bytes[1] == 0x49 && bytes[2] == 0x46 && bytes[3] == 0x46) {
            return "webp";
        }
        return "png";
    }

    private record NamedPattern(String name, Pattern pattern) {
    }
}
