package com.shaft.doctor.collect;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses SHAFT's single-file Allure "awesome" HTML report, recovering the per-test JSON payloads
 * that the report embeds so Doctor can analyze a shared {@code AllureReport.html} without also
 * requiring the original {@code allure-results} directory.
 *
 * <p>The report embeds every {@code data/*} and {@code widgets/*} payload as a base64-encoded
 * argument to repeated {@code d("name","base64")} calls inside inline {@code <script>} tags. This
 * parser reads the file once, scans it with a single linear regex pass (no re-scanning per match),
 * and decodes only the {@code data/test-results/*.json} entries.</p>
 */
final class AllureHtmlReportParser {
    /**
     * Marker that identifies a SHAFT single-file Allure 3 ("awesome") report. Present even before
     * the embedded data calls appear later in the file.
     */
    private static final String EMBEDDED_DATA_MARKER = "window.allureReportData";
    private static final String TEST_RESULT_PREFIX = "data/test-results/";
    private static final Pattern EMBEDDED_ENTRY_PATTERN = Pattern.compile("d\\(\"([^\"]*)\",\"([^\"]*)\"\\)");

    private final JsonMapper mapper = JsonMapper.builder().build();

    /**
     * One parsed embedded test result, plus how many sibling entries could not be decoded.
     *
     * @param testResults parsed embedded test-result JSON objects, in embedded (report) order
     * @param reportDetected whether the file contains the single-file report's embedded-data marker
     * @param malformedEntryCount count of {@code data/test-results/*.json} entries that were present
     *                            but could not be base64-decoded or parsed as a JSON object
     */
    record ParseResult(List<JsonNode> testResults, boolean reportDetected, int malformedEntryCount) {
        private static final ParseResult NOT_A_REPORT = new ParseResult(List.of(), false, 0);
    }

    /**
     * Parses the given file for embedded Allure test results.
     *
     * <p>Never throws: an unreadable file, a missing marker, or malformed embedded entries all
     * result in a {@link ParseResult} the caller can inspect, never an exception.</p>
     *
     * @param file candidate SHAFT single-file Allure HTML report
     * @return parse outcome
     */
    ParseResult parse(Path file) {
        String content;
        try {
            content = Files.readString(file, StandardCharsets.UTF_8);
        } catch (IOException | RuntimeException exception) {
            return ParseResult.NOT_A_REPORT;
        }
        if (!content.contains(EMBEDDED_DATA_MARKER)) {
            return ParseResult.NOT_A_REPORT;
        }

        List<JsonNode> results = new ArrayList<>();
        int malformed = 0;
        Matcher matcher = EMBEDDED_ENTRY_PATTERN.matcher(content);
        while (matcher.find()) {
            String name = matcher.group(1);
            if (!name.startsWith(TEST_RESULT_PREFIX) || !name.endsWith(".json")) {
                continue;
            }
            JsonNode decoded = decode(matcher.group(2));
            if (decoded != null) {
                results.add(decoded);
            } else {
                malformed++;
            }
        }
        return new ParseResult(List.copyOf(results), true, malformed);
    }

    private JsonNode decode(String base64) {
        try {
            byte[] bytes = Base64.getDecoder().decode(base64);
            JsonNode node = mapper.readTree(bytes);
            return node != null && node.isObject() ? node : null;
        } catch (RuntimeException exception) {
            return null;
        }
    }
}
