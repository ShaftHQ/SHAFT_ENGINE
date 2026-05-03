package com.shaft.api;

import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.shaft.driver.SHAFT;
import io.qameta.allure.Allure;
import io.restassured.filter.Filter;
import io.restassured.filter.FilterContext;
import io.restassured.response.Response;
import io.restassured.specification.FilterableRequestSpecification;
import io.restassured.specification.FilterableResponseSpecification;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

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
 * <p>This filter creates up to four attachment types per API call, each with the correct
 * MIME type for Allure 3:
 *
 * <ol>
 *   <li><b>Request</b> — {@code text/plain}; one attachment covering the HTTP method,
 *       URL, request headers, cookies, and (for small non-JSON/non-XML bodies) the
 *       request body inline.</li>
 *   <li><b>Request Body</b> (only when body is non-trivial JSON or XML) — the raw
 *       request body attached with its correct MIME type so Allure 3 can render it
 *       with syntax highlighting.</li>
 *   <li><b>Response</b> — {@code text/plain}; covers the HTTP status line, elapsed
 *       time, and response headers.</li>
 *   <li><b>Response Body</b> (only when body is non-empty) — the raw response body
 *       attached with the detected MIME type ({@code application/json},
 *       {@code text/xml}, or {@code text/plain}).</li>
 * </ol>
 *
 * <p>JSON bodies are pretty-printed before attachment to improve readability.
 *
 * <p>The threshold for inlining small bodies versus attaching them separately is
 * configurable via the {@code api.attachment.inline.body.threshold} property
 * (default: 500 characters); see {@code SHAFT.Properties.api.attachmentInlineBodyThreshold()}.
 *
 * @see RestActions#sendRequest(RestActions.RequestType, String,
 *      io.restassured.specification.RequestSpecification)
 */
public class ShaftRestAssuredFilter implements Filter {

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
        attachResponse(response);
        return response;
    }

    // ─── request attachment ────────────────────────────────────────────────────

    /**
     * Builds a human-readable {@code text/plain} summary of the outgoing request
     * (method, URL, headers, cookies) and, when the request body is non-trivial
     * JSON or XML, adds a separate body attachment with the correct MIME type.
     *
     * @param requestSpec the outgoing request specification
     */
    private void attachRequest(FilterableRequestSpecification requestSpec) {
        StringBuilder info = new StringBuilder();
        info.append(requestSpec.getMethod()).append(" ").append(requestSpec.getURI()).append("\n");

        // Headers
        if (requestSpec.getHeaders().size() > 0) {
            info.append("\nHeaders:\n");
            requestSpec.getHeaders().forEach(h ->
                    info.append("  ").append(h.getName()).append(": ").append(h.getValue()).append("\n"));
        }

        // Cookies
        if (requestSpec.getCookies().size() > 0) {
            info.append("\nCookies:\n");
            requestSpec.getCookies().forEach(c ->
                    info.append("  ").append(c.getName()).append("=").append(c.getValue()).append("\n"));
        }

        // Form params
        if (!requestSpec.getFormParams().isEmpty()) {
            info.append("\nForm Params:\n");
            requestSpec.getFormParams().forEach((k, v) ->
                    info.append("  ").append(k).append("=").append(v).append("\n"));
        }

        // Request body — inline if small and not JSON/XML, otherwise as a separate attachment
        Object body = requestSpec.getBody();
        if (body != null) {
            String bodyStr = body.toString();
            if (!bodyStr.isEmpty()) {
                String contentType = detectContentType(bodyStr,
                        requestSpec.getContentType() != null ? requestSpec.getContentType() : "");
                int inlineThreshold = SHAFT.Properties.api.attachmentInlineBodyThreshold();
                if (!"application/json".equals(contentType) && !"text/xml".equals(contentType)
                        && bodyStr.length() <= inlineThreshold) {
                    // Inline for small, non-JSON/XML bodies
                    info.append("\nBody:\n").append(bodyStr).append("\n");
                } else {
                    // Attach as a properly-typed file
                    String formatted = formatBody(bodyStr, contentType);
                    Allure.addAttachment("Request Body", contentType,
                            new ByteArrayInputStream(formatted.getBytes(StandardCharsets.UTF_8)),
                            getFileExtension(contentType));
                }
            }
        }

        Allure.addAttachment("Request", "text/plain",
                new ByteArrayInputStream(info.toString().getBytes(StandardCharsets.UTF_8)), ".txt");
    }

    // ─── response attachment ───────────────────────────────────────────────────

    /**
     * Builds a human-readable {@code text/plain} summary of the HTTP response
     * (status line, elapsed time, headers) and attaches the response body as a
     * separate attachment whose MIME type is detected from the content.
     *
     * @param response the HTTP response returned by the server
     */
    private void attachResponse(Response response) {
        StringBuilder info = new StringBuilder();
        info.append("Status: ").append(response.getStatusLine()).append("\n");
        info.append("Time: ").append(response.getTime()).append("ms\n");

        if (response.getHeaders().size() > 0) {
            info.append("\nHeaders:\n");
            response.getHeaders().forEach(h ->
                    info.append("  ").append(h.getName()).append(": ").append(h.getValue()).append("\n"));
        }

        Allure.addAttachment("Response", "text/plain",
                new ByteArrayInputStream(info.toString().getBytes(StandardCharsets.UTF_8)), ".txt");

        // Response body with proper MIME type
        String body = response.getBody().asString();
        if (body != null && !body.isEmpty()) {
            String declaredContentType = response.getContentType() != null ? response.getContentType() : "";
            String detectedContentType = detectContentType(body, declaredContentType);
            String formatted = formatBody(body, detectedContentType);
            Allure.addAttachment("Response Body", detectedContentType,
                    new ByteArrayInputStream(formatted.getBytes(StandardCharsets.UTF_8)),
                    getFileExtension(detectedContentType));
        }
    }

    // ─── helpers ──────────────────────────────────────────────────────────────

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
                return new GsonBuilder().setPrettyPrinting().create()
                        .toJson(JsonParser.parseString(body));
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
        return switch (contentType) {
            case "application/json" -> ".json";
            case "text/xml", "application/xml" -> ".xml";
            case "text/csv" -> ".csv";
            case "text/html" -> ".html";
            default -> ".txt";
        };
    }
}
