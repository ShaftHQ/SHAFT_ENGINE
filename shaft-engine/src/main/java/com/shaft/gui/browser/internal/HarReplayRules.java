package com.shaft.gui.browser.internal;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Builds browser network interception replay rules from a HAR (HTTP Archive) 1.2 file.
 *
 * <p>Matching is exact: a HAR entry replays only for requests whose HTTP method
 * (case-insensitive) and full request URL (including query string, case-sensitive) are identical
 * to the entry's recorded {@code request.method}/{@code request.url}. There is no path/query
 * normalization, body matching, or wildcarding, unlike
 * {@link com.shaft.tools.io.internal.HttpContractRecorder} contract replay. Duplicate entries for
 * the same method/URL are replayed in recorded order, one per matching request, mirroring the
 * contract replay cursor behavior.</p>
 *
 * <p>Entries without a {@code response} object, or with a non-positive {@code response.status},
 * are skipped. A {@code response.content.encoding} of {@code "base64"} is base64-decoded; any
 * other (or missing) encoding is treated as UTF-8 text. The {@code Content-Length} and
 * {@code Content-Encoding} response headers are dropped when replaying, because HAR
 * {@code response.content.text} is always already-decoded per the HAR spec, so passing along the
 * original transfer headers would misrepresent the reconstructed body.</p>
 */
public final class HarReplayRules {
    private static final ObjectMapper MAPPER = JsonMapper.builder().build();
    private static final Set<String> SKIPPED_RESPONSE_HEADERS = Set.of("content-length", "content-encoding");

    private HarReplayRules() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Builds one mock replay rule from every response-bearing entry in a HAR file.
     *
     * @param harFilePath path to a HAR 1.2 JSON file
     * @return replay rules that return recorded HAR responses
     */
    public static List<BrowserNetworkInterceptionRule> buildRules(String harFilePath) {
        List<HarEntry> replayable = readEntries(requireNonBlank(harFilePath, "harFilePath"));
        AtomicInteger cursor = new AtomicInteger();
        return List.of(BrowserNetworkInterceptionRule.mock(
                request -> replayable.stream()
                        .anyMatch(entry -> matches(entry, requestMethod(request), requestUrl(request))),
                request -> toBrowserResponse(nextReplayEntry(replayable, cursor, request))));
    }

    private static HarEntry nextReplayEntry(List<HarEntry> replayable, AtomicInteger cursor, HttpRequest request) {
        String method = requestMethod(request);
        String url = requestUrl(request);
        for (int index = cursor.get(); index < replayable.size(); index++) {
            HarEntry entry = replayable.get(index);
            if (matches(entry, method, url)) {
                cursor.set(index + 1);
                return entry;
            }
        }
        return replayable.stream()
                .filter(entry -> matches(entry, method, url))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("No HAR entry matched " + method + " " + url));
    }

    private static boolean matches(HarEntry entry, String method, String url) {
        return entry.method().equalsIgnoreCase(method) && entry.url().equals(url);
    }

    private static HttpResponse toBrowserResponse(HarEntry entry) {
        HttpResponse response = new HttpResponse().setStatus(entry.status());
        entry.headers().forEach(header -> {
            if (header.name != null && !SKIPPED_RESPONSE_HEADERS.contains(header.name.toLowerCase(Locale.ROOT))) {
                response.addHeader(header.name, value(header.value));
            }
        });
        if (entry.body().length > 0) {
            response.setContent(Contents.bytes(entry.body()));
        }
        return response;
    }

    private static List<HarEntry> readEntries(String harFilePath) {
        Har har = readHar(Path.of(harFilePath));
        List<HarEntry> entries = new ArrayList<>();
        for (HarEntryDto entryDto : entriesOf(har)) {
            HarEntry entry = toEntry(entryDto);
            if (entry != null) {
                entries.add(entry);
            }
        }
        return entries;
    }

    private static Har readHar(Path path) {
        try {
            return MAPPER.readValue(path.toFile(), Har.class);
        } catch (RuntimeException e) {
            throw new IllegalStateException("Could not read HAR file from " + path, e);
        }
    }

    private static List<HarEntryDto> entriesOf(Har har) {
        if (har == null || har.log == null || har.log.entries == null) {
            return List.of();
        }
        return har.log.entries;
    }

    private static HarEntry toEntry(HarEntryDto dto) {
        if (dto == null || dto.request == null || dto.response == null) {
            return null;
        }
        String method = value(dto.request.method);
        String url = value(dto.request.url);
        if (method.isBlank() || url.isBlank() || dto.response.status <= 0) {
            return null;
        }
        return new HarEntry(method.toUpperCase(Locale.ROOT), url, dto.response.status,
                headersOf(dto.response.headers), bodyOf(dto.response.content));
    }

    private static List<HarHeaderDto> headersOf(List<HarHeaderDto> headers) {
        return headers == null ? List.of() : headers;
    }

    private static byte[] bodyOf(HarContentDto content) {
        if (content == null || content.text == null) {
            return new byte[0];
        }
        if ("base64".equalsIgnoreCase(content.encoding)) {
            try {
                return Base64.getDecoder().decode(content.text);
            } catch (IllegalArgumentException e) {
                return content.text.getBytes(StandardCharsets.UTF_8);
            }
        }
        return content.text.getBytes(StandardCharsets.UTF_8);
    }

    private static String requestMethod(HttpRequest request) {
        return request == null || request.getMethod() == null ? "" : request.getMethod().name();
    }

    private static String requestUrl(HttpRequest request) {
        return request == null ? "" : value(request.getUri());
    }

    private static String requireNonBlank(String value, String fieldName) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(fieldName + " must not be blank.");
        }
        return value;
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    private record HarEntry(String method, String url, int status, List<HarHeaderDto> headers, byte[] body) {
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @SuppressWarnings("java:S1104")
    static class Har {
        public HarLog log;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @SuppressWarnings("java:S1104")
    static class HarLog {
        public List<HarEntryDto> entries;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @SuppressWarnings("java:S1104")
    static class HarEntryDto {
        public HarRequestDto request;
        public HarResponseDto response;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @SuppressWarnings("java:S1104")
    static class HarRequestDto {
        public String method;
        public String url;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @SuppressWarnings("java:S1104")
    static class HarResponseDto {
        public int status;
        public List<HarHeaderDto> headers;
        public HarContentDto content;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @SuppressWarnings("java:S1104")
    static class HarHeaderDto {
        public String name;
        public String value;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @SuppressWarnings("java:S1104")
    static class HarContentDto {
        public String text;
        public String encoding;
        public String mimeType;
    }
}
