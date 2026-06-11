package com.shaft.pilot.security;

import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.EvidenceReference;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Applies deterministic secret, field, and DOM redaction without retaining original values.
 */
public final class Redactor {
    public static final String REPLACEMENT = "[REDACTED]";
    private static final List<NamedPattern> BUILT_IN_PATTERNS = List.of(
            new NamedPattern("authorization-header",
                    Pattern.compile("(?im)(authorization|proxy-authorization)\\s*[:=]\\s*[^\\r\\n]+")),
            new NamedPattern("cookie-header",
                    Pattern.compile("(?im)(cookie|set-cookie)\\s*[:=]\\s*[^\\r\\n]+")),
            new NamedPattern("secret-assignment",
                    Pattern.compile("(?i)(password|passwd|secret|api[_-]?key|access[_-]?key|token)\\s*[:=]\\s*([^\\s,;]+)")),
            new NamedPattern("bearer-token", Pattern.compile("(?i)bearer\\s+[a-z0-9._~+/=-]+")),
            new NamedPattern("jwt", Pattern.compile("\\beyJ[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\b")),
            new NamedPattern("private-key",
                    Pattern.compile("(?s)-----BEGIN [A-Z ]*PRIVATE KEY-----.*?-----END [A-Z ]*PRIVATE KEY-----")),
            new NamedPattern("aws-access-key", Pattern.compile("\\b(?:AKIA|ASIA)[A-Z0-9]{16}\\b")));

    private Redactor() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Redacts primary and textual evidence content.
     *
     * @param request request to sanitize
     * @param policy configured policy
     * @return redacted content and safe summary
     */
    public static RedactionResult redact(AiRequest request, RedactionPolicy policy) {
        Set<String> appliedRules = new LinkedHashSet<>();
        Set<String> removedFields = new LinkedHashSet<>();
        String redactedText = redactText(request.text(), policy, appliedRules, removedFields);
        List<EvidenceReference> redactedEvidence = new ArrayList<>();
        for (EvidenceReference evidence : request.evidence()) {
            redactedEvidence.add(new EvidenceReference(evidence.id(), evidence.category(), evidence.mediaType(),
                    redactText(evidence.content(), policy, appliedRules, removedFields)));
        }
        String summary = "Applied " + appliedRules.size() + " redaction rule(s); removed "
                + removedFields.size() + " sensitive field name(s).";
        return new RedactionResult(redactedText, redactedEvidence, appliedRules, removedFields, summary);
    }

    private static String redactText(String input, RedactionPolicy policy, Set<String> appliedRules,
                                     Set<String> removedFields) {
        String output = input == null ? "" : input;
        if (looksLikeHtml(output)) {
            output = redactHtml(output, policy, appliedRules, removedFields);
        }
        for (NamedPattern namedPattern : BUILT_IN_PATTERNS) {
            String updated = namedPattern.pattern().matcher(output).replaceAll(REPLACEMENT);
            if (!updated.equals(output)) {
                appliedRules.add(namedPattern.name());
                output = updated;
            }
        }
        for (String expression : policy.customPatterns()) {
            if (expression == null || expression.isBlank()) {
                continue;
            }
            try {
                String updated = Pattern.compile(expression).matcher(output).replaceAll(REPLACEMENT);
                if (!updated.equals(output)) {
                    appliedRules.add("custom-pattern");
                    output = updated;
                }
            } catch (java.util.regex.PatternSyntaxException ignored) {
                appliedRules.add("invalid-custom-pattern");
            }
        }
        return output;
    }

    private static boolean looksLikeHtml(String value) {
        return value.indexOf('<') >= 0 && value.indexOf('>') > value.indexOf('<');
    }

    private static String redactHtml(String input, RedactionPolicy policy, Set<String> appliedRules,
                                     Set<String> removedFields) {
        Document document = Jsoup.parseBodyFragment(input);
        for (String selector : policy.selectors()) {
            if (selector == null || selector.isBlank()) {
                continue;
            }
            try {
                for (Element element : document.select(selector)) {
                    element.text(REPLACEMENT);
                    element.attributes().forEach(attribute -> attribute.setValue(REPLACEMENT));
                    appliedRules.add("dom-selector");
                }
            } catch (IllegalArgumentException ignored) {
                appliedRules.add("invalid-dom-selector");
            }
        }
        for (Element element : document.getAllElements()) {
            for (String attributeName : policy.sensitiveAttributes()) {
                if (element.hasAttr(attributeName)) {
                    element.attr(attributeName, REPLACEMENT);
                    removedFields.add(attributeName);
                    appliedRules.add("dom-attribute");
                }
            }
        }
        return document.body().html();
    }

    private record NamedPattern(String name, Pattern pattern) {
    }
}
