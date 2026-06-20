package com.shaft.gui.browser;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptionRule;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.RestValidationsBuilder;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.net.URI;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * Fluent builder for browser network request interception.
 */
public class NetworkInterceptionRequestBuilder {
    private final BrowserActions browserActions;
    private final List<Predicate<HttpRequest>> predicates = new ArrayList<>();
    private HttpMethod method;

    NetworkInterceptionRequestBuilder(BrowserActions browserActions) {
        this.browserActions = browserActions;
    }

    /**
     * Matches requests with the supplied HTTP method.
     *
     * @param method HTTP method to match
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder method(HttpMethod method) {
        this.method = Objects.requireNonNull(method, "method");
        return this;
    }

    /** @return this builder configured to match GET requests */
    public NetworkInterceptionRequestBuilder get() {
        return method(HttpMethod.GET);
    }

    /** @return this builder configured to match POST requests */
    public NetworkInterceptionRequestBuilder post() {
        return method(HttpMethod.POST);
    }

    /** @return this builder configured to match PUT requests */
    public NetworkInterceptionRequestBuilder put() {
        return method(HttpMethod.PUT);
    }

    /** @return this builder configured to match PATCH requests */
    public NetworkInterceptionRequestBuilder patch() {
        return method(HttpMethod.PATCH);
    }

    /** @return this builder configured to match DELETE requests */
    public NetworkInterceptionRequestBuilder delete() {
        return method(HttpMethod.DELETE);
    }

    /**
     * Matches requests whose full URL contains the given text.
     *
     * @param text URL fragment
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder urlContains(String text) {
        predicates.add(request -> request.getUri().contains(text));
        return this;
    }

    /**
     * Matches requests whose full URL matches the given regular expression.
     *
     * @param regex URL regular expression
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder urlMatches(String regex) {
        Pattern pattern = Pattern.compile(regex);
        predicates.add(request -> pattern.matcher(request.getUri()).matches());
        return this;
    }

    /**
     * Matches requests whose URL path equals the given value.
     *
     * @param path expected URL path
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder pathEquals(String path) {
        predicates.add(request -> path.equals(pathOf(request)));
        return this;
    }

    /**
     * Matches requests whose URL path contains the given text.
     *
     * @param text path fragment
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder pathContains(String text) {
        predicates.add(request -> pathOf(request).contains(text));
        return this;
    }

    /**
     * Matches requests that include the expected query parameter value.
     *
     * @param name  query parameter name
     * @param value expected value
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder queryParam(String name, String value) {
        predicates.add(request -> value.equals(queryParamsOf(request).get(name)));
        return this;
    }

    /**
     * Matches requests that include the expected header value.
     *
     * @param name  header name
     * @param value expected value
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder header(String name, String value) {
        predicates.add(request -> value.equals(request.getHeader(name)));
        return this;
    }

    /**
     * Matches requests whose body contains the given text.
     *
     * @param text body fragment
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder bodyContains(String text) {
        predicates.add(request -> request.contentAsString().contains(text));
        return this;
    }

    /**
     * Adds a custom request predicate.
     *
     * @param predicate predicate to apply
     * @return this builder
     */
    public NetworkInterceptionRequestBuilder matching(Predicate<HttpRequest> predicate) {
        predicates.add(Objects.requireNonNull(predicate, "predicate"));
        return this;
    }

    /**
     * Starts building a mocked response for matched requests.
     *
     * @return mocked response builder
     */
    public MockedResponseBuilder respond() {
        return new MockedResponseBuilder(this);
    }

    /**
     * Validates matched real responses with SHAFT hard assertions.
     *
     * @param validation response validation callback
     * @return the owning browser actions object
     */
    public BrowserActions assertResponse(Consumer<RestValidationsBuilder> validation) {
        return registerValidation(response -> validation.accept(Validations.assertThat().response(response)),
                "Configured network response assertion.");
    }

    /**
     * Validates matched real responses with SHAFT soft verifications.
     *
     * @param validation response validation callback
     * @return the owning browser actions object
     */
    public BrowserActions verifyResponse(Consumer<RestValidationsBuilder> validation) {
        return registerValidation(response -> validation.accept(Validations.verifyThat().response(response)),
                "Configured network response verification.");
    }

    private BrowserActions registerValidation(Consumer<io.restassured.response.Response> validation,
                                              String successMessage) {
        return browserActions.registerNetworkInterceptionRule(
                BrowserNetworkInterceptionRule.validate(buildPredicate(), validation), successMessage);
    }

    private Predicate<HttpRequest> buildPredicate() {
        return request -> {
            if (method != null && method != request.getMethod()) {
                return false;
            }
            for (Predicate<HttpRequest> predicate : predicates) {
                if (!predicate.test(request)) {
                    return false;
                }
            }
            return true;
        };
    }

    private static String pathOf(HttpRequest request) {
        try {
            return URI.create(request.getUri()).getPath();
        } catch (IllegalArgumentException e) {
            return "";
        }
    }

    private static Map<String, String> queryParamsOf(HttpRequest request) {
        try {
            String query = URI.create(request.getUri()).getRawQuery();
            if (query == null || query.isEmpty()) {
                return Map.of();
            }
            Map<String, String> parameters = new LinkedHashMap<>();
            for (String pair : query.split("&")) {
                int separator = pair.indexOf('=');
                String name = separator >= 0 ? pair.substring(0, separator) : pair;
                String value = separator >= 0 ? pair.substring(separator + 1) : "";
                parameters.put(urlDecode(name), urlDecode(value));
            }
            return parameters;
        } catch (IllegalArgumentException e) {
            return Map.of();
        }
    }

    private static String urlDecode(String value) {
        return URLDecoder.decode(value, StandardCharsets.UTF_8);
    }

    /**
     * Fluent mocked response builder.
     */
    public static class MockedResponseBuilder {
        private final NetworkInterceptionRequestBuilder requestBuilder;
        private final Map<String, String> headers = new LinkedHashMap<>();
        private int statusCode = 200;
        private byte[] body;

        private MockedResponseBuilder(NetworkInterceptionRequestBuilder requestBuilder) {
            this.requestBuilder = requestBuilder;
        }

        /**
         * Sets the HTTP status code.
         *
         * @param statusCode status code to return
         * @return this builder
         */
        public MockedResponseBuilder statusCode(int statusCode) {
            this.statusCode = statusCode;
            return this;
        }

        /**
         * Adds a response header.
         *
         * @param name  header name
         * @param value header value
         * @return this builder
         */
        public MockedResponseBuilder header(String name, String value) {
            headers.put(name, value);
            return this;
        }

        /**
         * Adds response headers.
         *
         * @param headers headers to add
         * @return this builder
         */
        public MockedResponseBuilder headers(Map<String, String> headers) {
            this.headers.putAll(headers);
            return this;
        }

        /**
         * Sets the response content type.
         *
         * @param contentType content type header value
         * @return this builder
         */
        public MockedResponseBuilder contentType(String contentType) {
            return header("Content-Type", contentType);
        }

        /**
         * Sets a UTF-8 string response body.
         *
         * @param body response body
         * @return this builder
         */
        public MockedResponseBuilder body(String body) {
            this.body = body.getBytes(StandardCharsets.UTF_8);
            return this;
        }

        /**
         * Sets a binary response body.
         *
         * @param body response body bytes
         * @return this builder
         */
        public MockedResponseBuilder body(byte[] body) {
            this.body = Arrays.copyOf(body, body.length);
            return this;
        }

        /**
         * Reads a UTF-8 response body from a file.
         *
         * @param relativeFilePath file path to read
         * @return this builder
         */
        public MockedResponseBuilder bodyFromFile(String relativeFilePath) {
            return body(FileActions.getInstance(true).readFile(relativeFilePath));
        }

        /**
         * Sets a JSON response body and content type.
         *
         * @param body JSON body
         * @return this builder
         */
        public MockedResponseBuilder jsonBody(String body) {
            return contentType("application/json").body(body);
        }

        /**
         * Registers the mocked response rule.
         *
         * @return the owning browser actions object
         */
        public BrowserActions perform() {
            return requestBuilder.browserActions.registerNetworkInterceptionRule(
                    BrowserNetworkInterceptionRule.mock(requestBuilder.buildPredicate(), request -> toResponse()),
                    "Configured network response mock.");
        }

        private HttpResponse toResponse() {
            HttpResponse response = new HttpResponse().setStatus(statusCode);
            headers.forEach(response::addHeader);
            if (body != null) {
                response.setContent(Contents.bytes(Arrays.copyOf(body, body.length)));
            }
            return response;
        }
    }
}
