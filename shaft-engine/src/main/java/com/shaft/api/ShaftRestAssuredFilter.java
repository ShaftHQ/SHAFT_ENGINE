package com.shaft.api;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.shaft.tools.io.internal.ReportContext;
import io.qameta.allure.Allure;
import io.restassured.filter.Filter;
import io.restassured.filter.FilterContext;
import io.restassured.response.Response;
import io.restassured.specification.FilterableRequestSpecification;
import io.restassured.specification.FilterableResponseSpecification;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * A SHAFT-native Allure filter for REST Assured that replaces the default
 * {@code AllureRestAssured} HTML-template-based filter.
 *
 * <p>The default {@code AllureRestAssured} filter (allure-rest-assured 2.x) uses a
 * Freemarker template whose renderer hard-codes the content type as {@code text/html}.
 * In Allure 3 the attachment is therefore treated as an HTML file, so users see raw
 * HTML source ({@code &lt;div&gt;}, {@code &lt;pre&gt;}, etc.) instead of the actual
 * request/response content — this is the "API attachments styled as HTML files" bug.
 *
 * <p>This filter creates exactly four attachments per API call, each with the correct
 * MIME type for Allure 3:
 *
 * <ol>
 *   <li><b>Request</b> — {@code text/plain}; HTTP method, URL, request headers, cookies,
 *       and form params (metadata only, no body inlined).</li>
 *   <li><b>Request Body</b> — always created when the request body is non-empty.
 *       The body is attached with its detected MIME type ({@code application/json},
 *       {@code text/xml}, or {@code text/plain}); JSON is pretty-printed for readability.</li>
 *   <li><b>Response</b> — {@code text/plain}; HTTP status line, elapsed time, and
 *       response headers.</li>
 *   <li><b>Response Body</b> — always created when the response body is non-empty.
 *       Binary responses (images, PDFs, etc.) are attached with their declared MIME type
 *       using the raw byte array; text responses are attached as UTF-8 strings with the
 *       appropriate text MIME type.</li>
 * </ol>
 *
 * <p>There is no size threshold for creating body attachments — every non-empty body
 * is always attached separately so it receives the correct MIME type and is rendered
 * with syntax highlighting in Allure 3.
 *
 * @see RestActions#sendRequest(RestActions.RequestType, String,
 *      io.restassured.specification.RequestSpecification)
 */
public class ShaftRestAssuredFilter implements Filter {
    private static final Gson PRETTY_GSON = new GsonBuilder().setPrettyPrinting().create();
    private static final String MASKED_VALUE = "********";
    private static final Pattern SENSITIVE_KEY_VALUE_PATTERN = Pattern.compile(
            "(?i)(\\b[\\w-]*(?:authorization|cookie|password|passwd|secret|token|apikey|api-key)[\\w-]*\\b\\s*[:=]\\s*)(\"[^\"]*\"|'[^']*'|[^&\\s,;]+)");
    private static final Pattern SENSITIVE_XML_ELEMENT_PATTERN = Pattern.compile(
            "(?is)(<([\\w:-]*(?:authorization|cookie|password|passwd|secret|token|apikey|api-key)[\\w:-]*)\\b[^>]*>)(.*?)(</\\2>)");

    /**
     * Intercepts every REST Assured request, attaches the request and response
     * information to the current Allure step/test, then returns the real response.
     *
     * @param requestSpec  the mutable request specification built by REST Assured
     * @param responseSpec the mutable response specification
     * @param filterContext used to pass the request to the next filter or to the
     *                      actual HTTP client
     * @return the HTTP response produced by the server
     */
    @Override
    public Response filter(FilterableRequestSpecification requestSpec,
                           FilterableResponseSpecification responseSpec,
                           FilterContext filterContext) {
        attachRequest(requestSpec);
        Response response = filterContext.next(requestSpec, responseSpec);
        attachResponse(response, labels(requestSpec));
        return response;
    }

    // ─── request attachment ────────────────────────────────────────────────────

    /**
     * Builds a human-readable {@code text/plain} summary of the outgoing request
     * (method, URL, headers, cookies) and always adds a separate "Request Body"
     * attachment with the correct MIME type whenever the body is non-empty.
     *
     * <p>Every non-empty request body is always attached as a dedicated file-based
     * attachment — there is no size threshold. This guarantees that JSON, XML, and
     * other typed bodies receive the correct MIME type and are rendered with syntax
     * highlighting in Allure 3.
     *
     * @param requestSpec the outgoing request specification
     */
    private void attachRequest(FilterableRequestSpecification requestSpec) {
        ApiEvidenceLabels labels = labels(requestSpec);
        StringBuilder info = new StringBuilder();
        info.append(labels.method()).append(" ").append(labels.path()).append("\n");

        // Headers
        if (requestSpec.getHeaders().size() > 0) {
            info.append("\nHeaders:\n");
            requestSpec.getHeaders().forEach(h ->
                    info.append("  ").append(h.getName()).append(": ")
                            .append(redactForReport(h.getName(), h.getValue())).append("\n"));
        }

        // Cookies
        if (requestSpec.getCookies().size() > 0) {
            info.append("\nCookies:\n");
            requestSpec.getCookies().forEach(c ->
                    info.append("  ").append(c.getName()).append("=").append(MASKED_VALUE).append("\n"));
        }

        // Form params
        if (!requestSpec.getFormParams().isEmpty()) {
            info.append("\nForm Params:\n");
            requestSpec.getFormParams().forEach((k, v) ->
                    info.append("  ").append(k).append("=").append(redactForReport(k, v)).append("\n"));
        }

        addAttachment("API Request - " + labels.requestTarget(), "text/plain",
                info.toString().getBytes(StandardCharsets.UTF_8), ".txt");

        // Request body — always attached separately (no threshold) so it receives the
        // correct MIME type and renders with syntax highlighting in Allure 3.
        Object body = requestSpec.getBody();
        if (body != null) {
            if (body instanceof byte[] binaryBody) {
                if (binaryBody.length > 0) {
                    String declaredType = requestSpec.getContentType() != null
                            ? requestSpec.getContentType() : "application/octet-stream";
                    String mimeType = normalizeMimeType(declaredType);
                    addAttachment("API Request Body - " + labels.requestTarget(), mimeType,
                            binaryBody,
                            getFileExtension(mimeType));
                }
            } else {
                String bodyStr = body.toString();
                if (!bodyStr.isEmpty()) {
                    String contentType = detectContentType(bodyStr,
                            requestSpec.getContentType() != null ? requestSpec.getContentType() : "");
                    String formatted = formatBody(redactBodyForReport(bodyStr, contentType), contentType);
                    addAttachment("API Request Body - " + labels.requestTarget(), contentType,
                            formatted.getBytes(StandardCharsets.UTF_8),
                            getFileExtension(contentType));
                }
            }
        }
    }

    // ─── response attachment ───────────────────────────────────────────────────

    /**
     * Builds a human-readable {@code text/plain} summary of the HTTP response
     * (status line, elapsed time, headers) and always adds a separate "Response Body"
     * attachment with the correct MIME type whenever the body is non-empty.
     *
     * <p>Binary response types (e.g. {@code image/*}, {@code application/pdf},
     * {@code application/octet-stream}) are attached using the raw byte array from the
     * response so the content is preserved exactly. Text-based types are decoded as
     * UTF-8 and formatted (JSON is pretty-printed).
     *
     * @param response the HTTP response returned by the server
     */
    private void attachResponse(Response response, ApiEvidenceLabels labels) {
        StringBuilder info = new StringBuilder();
        info.append("Status: ").append(response.getStatusLine()).append("\n");
        info.append("Time: ").append(response.getTime()).append("ms\n");

        if (response.getHeaders().size() > 0) {
            info.append("\nHeaders:\n");
            response.getHeaders().forEach(h ->
                    info.append("  ").append(h.getName()).append(": ")
                            .append(redactForReport(h.getName(), h.getValue())).append("\n"));
        }

        int statusCode = response.getStatusCode();
        String responseTarget = statusCode + " " + labels.requestTarget();
        addAttachment("API Response - " + responseTarget + " - " + response.getTime() + "ms", "text/plain",
                info.toString().getBytes(StandardCharsets.UTF_8), ".txt");

        // Response body — always attached separately (no threshold).
        // Binary types use byte[] to preserve the raw content; text types are decoded and formatted.
        String declaredContentType = response.getContentType() != null ? response.getContentType() : "";
        if (isBinaryContentType(declaredContentType)) {
            byte[] bodyBytes = response.getBody().asByteArray();
            if (bodyBytes != null && bodyBytes.length > 0) {
                String mimeType = normalizeMimeType(declaredContentType);
                addAttachment("API Response Body - " + responseTarget, mimeType,
                        bodyBytes,
                        getFileExtension(mimeType));
            }
        } else {
            String body = response.getBody().asString();
            if (body != null && !body.isEmpty()) {
                String detectedContentType = detectContentType(body, declaredContentType);
                String formatted = formatBody(redactBodyForReport(body, detectedContentType), detectedContentType);
                addAttachment("API Response Body - " + responseTarget, detectedContentType,
                        formatted.getBytes(StandardCharsets.UTF_8),
                        getFileExtension(detectedContentType));
            }
        }
    }

    // ─── helpers ──────────────────────────────────────────────────────────────

    private static String redactForReport(String key, Object value) {
        return isSensitiveKey(key) ? MASKED_VALUE : String.valueOf(value);
    }

    private static boolean isSensitiveKey(String key) {
        String normalizedKey = key == null ? "" : key.toLowerCase();
        return normalizedKey.contains("authorization")
                || normalizedKey.contains("cookie")
                || normalizedKey.contains("password")
                || normalizedKey.contains("passwd")
                || normalizedKey.contains("secret")
                || normalizedKey.contains("token")
                || normalizedKey.contains("apikey")
                || normalizedKey.contains("api-key");
    }

    private static ApiEvidenceLabels labels(FilterableRequestSpecification requestSpec) {
        String method = requestSpec.getMethod() == null ? "REQUEST" : requestSpec.getMethod();
        String path = sanitizedPath(requestSpec.getURI());
        return new ApiEvidenceLabels(method, path);
    }

    private static String sanitizedPath(String uri) {
        if (uri == null || uri.isBlank()) {
            return "/";
        }
        try {
            URI parsed = new URI(uri);
            String path = parsed.getRawPath();
            return path == null || path.isBlank() ? "/" : path;
        } catch (URISyntaxException e) {
            int queryIndex = uri.indexOf('?');
            String path = queryIndex >= 0 ? uri.substring(0, queryIndex) : uri;
            return path.isBlank() ? "/" : path;
        }
    }

    private static void addAttachment(String name, String contentType, byte[] content, String fileExtension) {
        Allure.addAttachment(name, contentType, new ByteArrayInputStream(content), fileExtension);
        ReportContext.recordAttachment(name, contentType, fileExtension, "api", content.length);
    }

    private String redactBodyForReport(String body, String contentType) {
        if (body == null || body.isEmpty()) {
            return body;
        }
        if ("application/json".equals(contentType) || looksLikeJson(body)) {
            try {
                return PRETTY_GSON.toJson(redactJson(JsonParser.parseString(body)));
            } catch (JsonParseException ignored) {
                // Fall back to text redaction if the body only looks like JSON.
            }
        }
        String redacted = SENSITIVE_KEY_VALUE_PATTERN.matcher(body).replaceAll("$1" + MASKED_VALUE);
        return SENSITIVE_XML_ELEMENT_PATTERN.matcher(redacted).replaceAll("$1" + MASKED_VALUE + "$4");
    }

    private static JsonElement redactJson(JsonElement element) {
        if (element == null || element.isJsonNull()) {
            return element;
        }
        if (element.isJsonObject()) {
            JsonObject redacted = new JsonObject();
            for (Map.Entry<String, JsonElement> entry : element.getAsJsonObject().entrySet()) {
                redacted.add(entry.getKey(), isSensitiveKey(entry.getKey())
                        ? new JsonPrimitive(MASKED_VALUE)
                        : redactJson(entry.getValue()));
            }
            return redacted;
        }
        if (element.isJsonArray()) {
            JsonArray redacted = new JsonArray();
            for (JsonElement child : element.getAsJsonArray()) {
                redacted.add(redactJson(child));
            }
            return redacted;
        }
        return element.deepCopy();
    }

    /**
     * Determines the best MIME type for an attachment body.
     *
     * <p>The declared {@code Content-Type} header value is checked first.  If it
     * signals JSON ({@code application/json} or any {@code *\/json*} variant) or
     * XML, the corresponding standard MIME type is returned directly.
     *
     * <p>When the declared type is {@code text/html} but the body is actually JSON
     * (some APIs serve JSON with the wrong content-type header), the method falls
     * back to content-sniffing so that Allure 3 renders it with syntax highlighting.
     *
     * @param body                the raw body string to sniff if necessary
     * @param declaredContentType the {@code Content-Type} header value (may be empty)
     * @return the recommended MIME type for the attachment
     */
    public String detectContentType(String body, String declaredContentType) {
        if (declaredContentType != null && !declaredContentType.isEmpty()) {
            String lower = declaredContentType.toLowerCase();
            if (lower.contains("json")) return "application/json";
            if (lower.contains("xml")) return "text/xml";
            if (lower.contains("csv")) return "text/csv";
            if (lower.contains("html")) {
                // Some APIs declare text/html but actually return JSON — sniff the body for JSON only.
                // Do NOT sniff for XML: genuine HTML starts with '<' just like XML, so the sniff
                // would incorrectly reclassify HTML as text/xml.
                if (looksLikeJson(body)) return "application/json";
                return "text/html";
            }
        }
        // Content-type absent or generic — sniff from body
        if (looksLikeJson(body)) return "application/json";
        if (looksLikeXml(body)) return "text/xml";
        return "text/plain";
    }

    /**
     * Returns {@code true} when the declared {@code Content-Type} indicates binary
     * (non-text) content that must be preserved as a byte array rather than decoded
     * as a UTF-8 string.
     *
     * <p>Binary types include {@code image/*}, {@code audio/*}, {@code video/*},
     * {@code application/pdf}, {@code application/zip}, {@code application/octet-stream},
     * and common compressed/archive formats.
     *
     * @param contentType the declared {@code Content-Type} header value
     * @return {@code true} if the content type represents binary data
     */
    public boolean isBinaryContentType(String contentType) {
        if (contentType == null || contentType.isEmpty()) return false;
        String lower = contentType.toLowerCase();
        return lower.startsWith("image/")
                || lower.startsWith("audio/")
                || lower.startsWith("video/")
                || lower.contains("octet-stream")
                || lower.contains("application/pdf")
                || lower.contains("application/zip")
                || lower.contains("application/x-zip")
                || lower.contains("application/gzip")
                || lower.contains("application/vnd.openxmlformats")
                || lower.contains("application/vnd.ms-");
    }

    /**
     * Strips charset and parameter suffixes from a {@code Content-Type} header value
     * and returns the bare MIME type.
     *
     * <p>For example, {@code "application/json; charset=utf-8"} becomes
     * {@code "application/json"}.
     *
     * @param contentType the raw {@code Content-Type} header value
     * @return the bare MIME type without parameters, or the original string if no
     *         semicolon is present
     */
    public String normalizeMimeType(String contentType) {
        if (contentType == null || contentType.isEmpty()) return "application/octet-stream";
        int semicolon = contentType.indexOf(';');
        return (semicolon >= 0 ? contentType.substring(0, semicolon) : contentType).trim();
    }

    /**
     * Returns {@code true} when {@code content} looks like a JSON object or array.
     *
     * <p>Performs a two-stage check: first a lightweight bracket-boundary heuristic,
     * then actual Gson parsing to confirm the content is valid JSON.  This eliminates
     * false positives such as {@code "{not valid json}"} or {@code "[plain text]"}
     * while keeping the fast-path rejection for non-JSON content.
     *
     * @param content the string to inspect
     * @return {@code true} if {@code content} is a syntactically valid JSON object
     *         or array
     */
    public boolean looksLikeJson(String content) {
        String trimmed = content.trim();
        if ((trimmed.startsWith("{") && trimmed.endsWith("}"))
                || (trimmed.startsWith("[") && trimmed.endsWith("]"))) {
            try {
                JsonParser.parseString(trimmed);
                return true;
            } catch (JsonParseException ignored) {
                // Not parseable — content only superficially resembles JSON
            }
        }
        return false;
    }

    /**
     * Returns {@code true} when {@code content} looks like an XML document or fragment.
     *
     * <p>Uses a lightweight structural check: XML must start with {@code <} and contain
     * either a closing tag ({@code </}) or a self-closing element ({@code />}).  This
     * avoids false positives for arbitrary strings that merely start with {@code <} and
     * end with {@code >} (e.g. {@code "<not xml at all>"}).
     *
     * <p><strong>Note:</strong> HTML documents also pass this check because they contain
     * closing tags.  In practice, HTML bodies are accompanied by a {@code Content-Type:
     * text/html} header which is resolved before this sniff is invoked, so the
     * misclassification scenario is rare.
     *
     * @param content the string to inspect
     * @return {@code true} if the trimmed content has the structural markers of XML
     */
    public boolean looksLikeXml(String content) {
        String trimmed = content.trim();
        return trimmed.startsWith("<") && (trimmed.contains("</") || trimmed.contains("/>"));
    }

    /**
     * Pretty-prints JSON bodies so they are immediately readable in Allure 3's
     * JSON viewer; returns all other bodies unchanged.
     *
     * @param body        the raw body content
     * @param contentType the MIME type already determined for the body
     * @return pretty-printed JSON string, or the original body for non-JSON content
     */
    public String formatBody(String body, String contentType) {
        if ("application/json".equals(contentType)) {
            try {
                return PRETTY_GSON.toJson(JsonParser.parseString(body));
            } catch (JsonParseException ignored) {
                // If parsing fails return the original
            }
        }
        return body;
    }

    /**
     * Returns the file extension that corresponds to the given MIME type.
     *
     * @param contentType a standard MIME type string
     * @return a dotted file extension such as {@code ".json"} or {@code ".txt"}
     */
    public String getFileExtension(String contentType) {
        if (contentType == null) return ".txt";
        String lower = contentType.toLowerCase();
        if (lower.contains("json")) return ".json";
        if (lower.contains("xml")) return ".xml";
        if (lower.contains("csv")) return ".csv";
        if (lower.contains("html")) return ".html";
        if (lower.startsWith("image/png")) return ".png";
        if (lower.startsWith("image/gif")) return ".gif";
        if (lower.startsWith("image/jpeg") || lower.startsWith("image/jpg")) return ".jpg";
        if (lower.startsWith("image/")) return ".png";
        if (lower.startsWith("video/mp4")) return ".mp4";
        if (lower.startsWith("video/")) return ".mp4";
        if (lower.contains("pdf")) return ".pdf";
        if (lower.contains("zip")) return ".zip";
        if (lower.contains("octet-stream")) return ".bin";
        if (lower.contains("vnd.openxmlformats-officedocument.spreadsheetml")) return ".xlsx";
        if (lower.contains("vnd.openxmlformats-officedocument.wordprocessingml")) return ".docx";
        if (lower.contains("vnd.openxmlformats-officedocument.presentationml")) return ".pptx";
        return ".txt";
    }

    private record ApiEvidenceLabels(String method, String path) {
        String requestTarget() {
            return method + " " + path;
        }
    }
}
