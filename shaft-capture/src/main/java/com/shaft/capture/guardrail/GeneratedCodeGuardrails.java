package com.shaft.capture.guardrail;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Lexical guardrail rules for generated or agent-authored SHAFT test code, shared by any caller
 * (MCP tools, capture code generation) that needs to check generated code without depending on
 * shaft-mcp.
 */
public final class GeneratedCodeGuardrails {
    private static final Pattern THREAD_SLEEP = Pattern.compile("\\bThread\\s*\\.\\s*sleep\\s*\\(");
    private static final Pattern SHAFT_LOCATOR_XPATH = Pattern.compile(
            "\\bSHAFT\\s*\\.\\s*GUI\\s*\\.\\s*Locator\\s*\\.\\s*xpath\\s*\\(");
    private static final Pattern SMART_LOCATOR = Pattern.compile(
            "\\bSHAFT\\s*\\.\\s*GUI\\s*\\.\\s*Locator\\s*\\.\\s*(?:clickableField|inputField)\\s*\\(");
    // Issue #4239 P1.1: with CaptureGenerator's emission-time ladder (rung 1 verified ARIA role,
    // rung 2 self-verified XPath, rung 3 FAILED) now the primary enforcement, this rule is the
    // regression-catching backstop -- unconditional ERROR, no "only when an ARIA candidate existed"
    // qualifier (unimplementable in a text lint: it cannot see recorded candidates, only emitted
    // code). Deliberately excludes hasTagName/hasRole -- the SHAFT locator BUILDER methods, not these
    // raw Selenium-strategy factories.
    private static final Pattern NON_ARIA_LOCATOR = Pattern.compile(
            "\\bSHAFT\\s*\\.\\s*GUI\\s*\\.\\s*Locator\\s*\\.\\s*(?:id|name|cssSelector|className|tagName)\\s*\\(");
    private static final Pattern CLASS_BOUNDARY = Pattern.compile("\\bclass\\s+\\w+");
    private static final Pattern TEST_ANNOTATION = Pattern.compile("@Test\\b");
    private static final Pattern LOCATOR_DECLARATION = Pattern.compile(
            "\\bBy\\s+\\w+\\s*=\\s*(?:SHAFT\\s*\\.\\s*GUI\\s*\\.\\s*Locator\\s*\\.|By\\s*\\.\\s*xpath\\s*\\()");
    private static final Pattern BY_XPATH = Pattern.compile("\\bBy\\s*\\.\\s*xpath\\s*\\(\\s*\"((?:\\\\.|[^\"\\\\])*)\"");
    private static final Pattern PAGE_FACTORY = Pattern.compile("(?:@FindBy\\b|\\bPageFactory\\b)");
    private static final Pattern IMPLICIT_WAIT = Pattern.compile(
            "\\.\\s*manage\\s*\\(\\s*\\)\\s*\\.\\s*timeouts\\s*\\(\\s*\\)\\s*\\.\\s*implicitlyWait\\s*\\(");
    private static final Pattern RAW_FIND_ELEMENT = Pattern.compile("\\bdriver\\s*\\.\\s*findElements?\\s*\\(");
    private static final Pattern HEADED_BROWSER = Pattern.compile(
            "(?:\\.\\s*setHeadless\\s*\\(\\s*false\\s*\\)|--headed\\b|--headless\\s*=\\s*false|\\bheadless\\s*=\\s*false\\b)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern DIRECT_SYSTEM_PROPERTY = Pattern.compile("\\bSystem\\s*\\.\\s*getProperty\\s*\\(");
    private static final Pattern HARDCODED_SECRET_ASSIGNMENT = Pattern.compile(
            "\\b\\w*(?:password|passwd|pwd|secret|token|apikey|api_key|authorization|clientsecret|client_secret"
                    + "|accesstoken|access_token)\\w*\\s*=\\s*\"((?:\\\\.|[^\"\\\\]){8,})\"",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern HARDCODED_SECRET_HEADER = Pattern.compile(
            "\\b(?:put|header|setHeader|addHeader)\\s*\\(\\s*\"[^\"]*(?:authorization|token|password|secret"
                    + "|api[-_ ]?key)[^\"]*\"\\s*,\\s*\"((?:\\\\.|[^\"\\\\]){8,})\"",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern HARDCODED_SECRET_SETTER = Pattern.compile(
            "\\b(?:setPassword|setToken|setSecret|setApiKey|setAuthorization|bearerToken|basicAuth)\\s*\\("
                    + "\\s*\"((?:\\\\.|[^\"\\\\]){8,})\"",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern STRING_LITERAL = Pattern.compile("\"(?:\\\\.|[^\"\\\\])*\"");
    private static final String NO_SLEEP = "Do not generate Thread.sleep; use SHAFT waits/actions/assertions.";
    private static final String NO_ABSOLUTE_XPATH = "Do not generate absolute XPath; prefer role-based locators,"
            + " then the SHAFT.GUI.Locator XPath builder.";
    private static final String NO_SHAFT_LOCATOR_XPATH = "Do not generate SHAFT.GUI.Locator.xpath; use role-based"
            + " locators, the SHAFT locator builder, or By.xpath only as a last fallback.";
    private static final String NO_IMPLICIT_WAIT = "Avoid Selenium implicit waits; use SHAFT waits/actions/assertions.";
    private static final String NO_RAW_FIND_ELEMENT = "Avoid direct driver.findElement/findElements calls in generated"
            + " SHAFT tests; route actions through SHAFT facades or page objects.";
    private static final String NO_HEADED_BROWSER = "Do not hard-code headed browser setup; keep generated tests"
            + " headless-configurable.";
    private static final String NO_DIRECT_SYSTEM_PROPERTY = "Avoid direct System.getProperty() in SHAFT-like snippets;"
            + " use SHAFT properties or injected configuration.";
    private static final String NO_HARDCODED_SECRET = "Do not hard-code obvious header, token, or password secrets.";
    private static final String NO_SMART_LOCATOR = "Avoid generating SHAFT.GUI.Locator.clickableField/inputField"
            + " (intent-based smart locators); prefer role-based locators or the SHAFT locator builder instead.";
    private static final String NO_NON_ARIA_LOCATOR = "Do not generate SHAFT.GUI.Locator.id/name/cssSelector/"
            + "className/tagName; use a unique author-written id through the locator builder "
            + "(hasAnyTagName().hasId(...)), else a self-verified ARIA role (hasRole), else a self-verified "
            + "relative By.xpath.";
    private static final String NO_POM_VIOLATION = "Do not declare locators (SHAFT.GUI.Locator.* or By.xpath) inside a"
            + " class that also has @Test methods; move locators/actions into a Page Object class and keep"
            + " orchestration in the test.";

    private GeneratedCodeGuardrails() {
    }

    /**
     * Checks generated or agent-authored code for SHAFT guardrails.
     *
     * @param code source code to check
     * @return guardrail check result
     */
    public static GuardrailCheckResult check(String code) {
        String source = code == null ? "" : code;
        List<GuardrailViolation> violations = new ArrayList<>();
        addThreadSleepViolations(source, violations);
        addShaftLocatorXpathViolations(source, violations);
        addSmartLocatorViolations(source, violations);
        addNonAriaLocatorViolations(source, violations);
        addPageObjectModelViolations(source, violations);
        addAbsoluteXpathViolations(source, violations);
        addPageFactoryWarnings(source, violations);
        addImplicitWaitWarnings(source, violations);
        addRawFindElementWarnings(source, violations);
        addHeadedBrowserWarnings(source, violations);
        addDirectSystemPropertyWarnings(source, violations);
        addHardcodedSecretViolations(source, violations);
        boolean passed = violations.stream().noneMatch(violation -> "ERROR".equals(violation.severity()));
        return new GuardrailCheckResult(passed, violations);
    }

    private static void addThreadSleepViolations(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, THREAD_SLEEP, "THREAD_SLEEP", "ERROR", NO_SLEEP);
    }

    private static void addShaftLocatorXpathViolations(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, SHAFT_LOCATOR_XPATH, "SHAFT_LOCATOR_XPATH", "ERROR",
                NO_SHAFT_LOCATOR_XPATH);
    }

    private static void addSmartLocatorViolations(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, SMART_LOCATOR, "SMART_LOCATOR", "ERROR", NO_SMART_LOCATOR);
    }

    private static void addNonAriaLocatorViolations(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, NON_ARIA_LOCATOR, "NON_ARIA_LOCATOR", "ERROR", NO_NON_ARIA_LOCATOR);
    }

    private static void addPageObjectModelViolations(String source, List<GuardrailViolation> violations) {
        List<Integer> boundaries = classBoundaries(source);
        for (int i = 0; i < boundaries.size() - 1; i++) {
            int start = boundaries.get(i);
            String classBody = source.substring(start, boundaries.get(i + 1));
            Matcher locatorMatcher = LOCATOR_DECLARATION.matcher(classBody);
            if (!locatorMatcher.find() || !TEST_ANNOTATION.matcher(classBody).find()) {
                continue;
            }
            int offset = start + locatorMatcher.start();
            if (isCommentOnlyLine(source, offset)) {
                continue;
            }
            violations.add(violation("POM_VIOLATION", "ERROR", NO_POM_VIOLATION, source, offset));
        }
    }

    private static List<Integer> classBoundaries(String source) {
        List<Integer> boundaries = new ArrayList<>();
        Matcher classMatcher = CLASS_BOUNDARY.matcher(source);
        while (classMatcher.find()) {
            boundaries.add(classMatcher.start());
        }
        boundaries.add(source.length());
        return boundaries;
    }

    private static void addAbsoluteXpathViolations(String source, List<GuardrailViolation> violations) {
        Matcher matcher = BY_XPATH.matcher(source);
        while (matcher.find()) {
            if (isCommentOnlyLine(source, matcher.start())) {
                continue;
            }
            String xpath = matcher.group(1).replace("\\\"", "\"").trim();
            if (isAbsoluteXpath(xpath)) {
                violations.add(violation(
                        "ABSOLUTE_XPATH",
                        "ERROR",
                        NO_ABSOLUTE_XPATH,
                        source,
                        matcher.start()));
            }
        }
    }

    private static void addPageFactoryWarnings(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, PAGE_FACTORY, "PAGE_FACTORY", "WARNING",
                "Prefer Selenium By objects and SHAFT.GUI.Locator instead of @FindBy or PageFactory.");
    }

    private static void addImplicitWaitWarnings(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, IMPLICIT_WAIT, "IMPLICIT_WAIT", "WARNING", NO_IMPLICIT_WAIT);
    }

    private static void addRawFindElementWarnings(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, RAW_FIND_ELEMENT, "RAW_FIND_ELEMENT", "WARNING", NO_RAW_FIND_ELEMENT);
    }

    private static void addHeadedBrowserWarnings(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, HEADED_BROWSER, "HEADED_BROWSER", "WARNING", NO_HEADED_BROWSER);
    }

    private static void addDirectSystemPropertyWarnings(String source, List<GuardrailViolation> violations) {
        addPatternViolations(source, violations, DIRECT_SYSTEM_PROPERTY, "DIRECT_SYSTEM_PROPERTY", "WARNING",
                NO_DIRECT_SYSTEM_PROPERTY);
    }

    private static void addHardcodedSecretViolations(String source, List<GuardrailViolation> violations) {
        addSecretViolations(source, violations, HARDCODED_SECRET_ASSIGNMENT);
        addSecretViolations(source, violations, HARDCODED_SECRET_HEADER);
        addSecretViolations(source, violations, HARDCODED_SECRET_SETTER);
    }

    private static void addPatternViolations(
            String source,
            List<GuardrailViolation> violations,
            Pattern pattern,
            String kind,
            String severity,
            String message) {
        Matcher matcher = pattern.matcher(source);
        while (matcher.find()) {
            if (isCommentOnlyLine(source, matcher.start())) {
                continue;
            }
            violations.add(violation(kind, severity, message, source, matcher.start()));
        }
    }

    private static void addSecretViolations(
            String source,
            List<GuardrailViolation> violations,
            Pattern pattern) {
        Matcher matcher = pattern.matcher(source);
        while (matcher.find()) {
            if (isCommentOnlyLine(source, matcher.start()) || !isSuspiciousSecretLiteral(matcher.group(1))) {
                continue;
            }
            violations.add(secretViolation(source, matcher.start()));
        }
    }

    private static boolean isAbsoluteXpath(String xpath) {
        return (xpath.startsWith("/") && !xpath.startsWith("//")) || xpath.startsWith("(/");
    }

    private static boolean isSuspiciousSecretLiteral(String literal) {
        String value = literal == null ? "" : literal.trim();
        if (isTooShortOrTemplatePlaceholder(value)) {
            return false;
        }
        String lower = value.toLowerCase(Locale.ROOT);
        if (looksLikePlaceholderWord(lower)) {
            return false;
        }
        String compact = lower.replaceAll("[^a-z0-9]+", "");
        return !isGenericSecretKeywordOnly(compact);
    }

    private static boolean isTooShortOrTemplatePlaceholder(String value) {
        return value.length() < 8 || value.contains("${");
    }

    private static boolean looksLikePlaceholderWord(String lower) {
        return lower.contains("example") || lower.contains("sample") || lower.contains("dummy")
                || lower.contains("placeholder") || lower.contains("redacted") || lower.contains("changeme")
                || lower.contains("change-me") || lower.contains("your_") || lower.contains("your-");
    }

    private static boolean isGenericSecretKeywordOnly(String compact) {
        return switch (compact) {
            case "authorization", "apikey", "token", "accesstoken", "password", "passwd", "pwd", "secret",
                    "clientsecret" -> true;
            default -> false;
        };
    }

    private static boolean isCommentOnlyLine(String source, int offset) {
        int lineStart = source.lastIndexOf('\n', Math.max(0, offset - 1)) + 1;
        String prefix = source.substring(lineStart, Math.min(offset, source.length())).trim();
        return prefix.startsWith("//") || prefix.startsWith("/*") || prefix.startsWith("*");
    }

    private static GuardrailViolation violation(
            String kind,
            String severity,
            String message,
            String source,
            int offset) {
        return new GuardrailViolation(kind, severity, message, lineNumber(source, offset), lineSnippet(source, offset));
    }

    private static GuardrailViolation secretViolation(String source, int offset) {
        return new GuardrailViolation(
                "HARDCODED_SECRET",
                "ERROR",
                NO_HARDCODED_SECRET,
                lineNumber(source, offset),
                STRING_LITERAL.matcher(lineSnippet(source, offset)).replaceAll("\"[REDACTED]\""));
    }

    private static int lineNumber(String source, int offset) {
        int line = 1;
        int length = Math.min(offset, source.length());
        for (int index = 0; index < length; index++) {
            if (source.charAt(index) == '\n') {
                line++;
            }
        }
        return line;
    }

    private static String lineSnippet(String source, int offset) {
        int start = source.lastIndexOf('\n', Math.max(0, offset - 1)) + 1;
        int end = source.indexOf('\n', offset);
        if (end < 0) {
            end = source.length();
        }
        String snippet = source.substring(start, end).trim();
        return snippet.length() > 160 ? snippet.substring(0, 157) + "..." : snippet;
    }
}
