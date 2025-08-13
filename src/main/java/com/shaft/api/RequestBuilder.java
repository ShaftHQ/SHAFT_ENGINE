package com.shaft.api;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import io.qameta.allure.Step;
import io.restassured.config.RestAssuredConfig;
import io.restassured.config.SSLConfig;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import lombok.AccessLevel;
import lombok.Getter;

import java.util.*;
import java.util.concurrent.TimeUnit;

import static io.restassured.RestAssured.config;

@Getter(AccessLevel.PACKAGE) //for unit tests
@SuppressWarnings("unused")
public class RequestBuilder {
    @Getter
    private static final Map<String, List<Double>> performanceData = new HashMap<>();
    private RestActions session;
    private Map<String, String> sessionHeaders;
    private Map<String, Object> sessionCookies;
    private RestAssuredConfig sessionConfig;
    private RestActions.RequestType requestType;
    private String serviceName;
    private String serviceURI;
    private int targetStatusCode;
    private String urlArguments = null;
    private List<List<Object>> parameters = null;
    private Map<String, Object> parametersMap = null;
    private RestActions.ParametersType parametersType = null;
    private Object requestBody = null;
    private ContentType contentType = null;
    private AuthenticationType authenticationType;
    private String authenticationUsername;
    private String authenticationPassword;
    private boolean appendDefaultContentCharsetToContentTypeIfUndefined;
    private boolean urlEncodingEnabled;

    /**
     * Start building a new API request.
     *
     * @param serviceURI  the base URI of the webservice that you want to hit
     * @param serviceName the path/name of the webservice that you want to hit {/servicePATH/serviceNAME}
     * @param requestType the type of your API request {POST, GET, PATCH, DELETE, PUT}
     */
    RequestBuilder(String serviceURI, String serviceName, RestActions.RequestType requestType) {
        initializeVariables(new RestActions(serviceURI), serviceName, requestType);
    }

    /**
     * Start building a new API request from an existing RestActions session.
     *
     * @param session     the RestActions session that contains your serviceURI/cookies/headers
     * @param serviceName the path/name of the webservice that you want to hit {/servicePATH/serviceNAME}
     * @param requestType the type of your API request {POST, GET, PATCH, DELETE, PUT}
     */
    RequestBuilder(RestActions session, String serviceName, RestActions.RequestType requestType) {
        initializeVariables(session, serviceName, requestType);
    }

    private void initializeVariables(RestActions session, String serviceName, RestActions.RequestType requestType) {
        this.session = session;
        this.serviceURI = session.getServiceURI();
        this.sessionCookies = session.getSessionCookies();
        this.sessionHeaders = session.getSessionHeaders();
        this.sessionConfig = session.getSessionConfig();
        this.requestType = requestType;
        this.serviceName = serviceName;
        this.targetStatusCode = 0;
        this.contentType = ContentType.ANY;
        this.authenticationType = AuthenticationType.NONE;
        this.appendDefaultContentCharsetToContentTypeIfUndefined = true;
        this.urlEncodingEnabled = true;
    }

    /**
     * set useRelaxedHTTPSValidation configuration to trust all hosts regardless if the SSL certificate is invalid in the request builder
     * 'SSL' is the protocol name by default
     *
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder useRelaxedHTTPSValidation() {
        useRelaxedHTTPSValidation("SSL");
        return this;
    }

    /**
     * set useRelaxedHTTPSValidation configuration to trust all hosts regardless if the SSL certificate is invalid in the request builder
     * *
     *
     * @param protocol The standard name of the requested protocol.
     * @return a self-reference to be used to continue building your API request
     */
    @SuppressWarnings("UnusedReturnValue")
    public RequestBuilder useRelaxedHTTPSValidation(String protocol) {
        this.sessionConfig = config().and().sslConfig(SSLConfig.sslConfig().relaxedHTTPSValidation(protocol));
        return this;
    }

    /**
     * Sets the expected target status code for the API request that you're currently building. By default, this value is set to any number between 200 and 299 which means that the request was successful as per the <a href="https://www.w3.org/Protocols/HTTP/HTRESP.html">W3C Standard documentation</a>.
     *
     * @param targetStatusCode the expected target status code.
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setTargetStatusCode(int targetStatusCode) {
        this.targetStatusCode = targetStatusCode;
        return this;
    }

    /**
     * Sets the url arguments (if any) for the API request that you're currently building. A request usually has only one of the following: urlArguments, parameters+type, or body
     *
     * @param urlArguments '&amp;' separated arguments without a preceding '?', is nullable, Example: "username=test&amp;password=test"
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setUrlArguments(String urlArguments) {
        this.urlArguments = urlArguments;
        return this;
    }

    /**
     * Dynamically sets path parameters by replacing placeholders in the `serviceName` using key-value pairs.
     *
     * @param paramMap a Map where the keys are the placeholder names (without curly braces) and the values are the replacement values.
     * @return the current instance of RequestBuilder for method chaining.
     * @throws IllegalArgumentException if any placeholder in the Map is not found in the `serviceName`.
     */
    public RequestBuilder setPathParameters(Map<String, Object> paramMap) {
        for (Map.Entry<String, Object> entry : paramMap.entrySet()) {
            String key = entry.getKey();
            String value = String.valueOf(entry.getValue());
            if (serviceName.contains("{" + key + "}")) {
                serviceName = serviceName.replace("{" + key + "}", value);
            } else {
                throw new IllegalArgumentException(
                        "Path parameter {" + key + "} not found in the serviceName: " + serviceName
                );
            }
        }
        return this;
    }

    /**
     * Dynamically sets path parameters by replacing placeholders in the `serviceName` using ordered string values.
     *
     * @param values the string values to replace placeholders in the `serviceName`, provided in the exact order of their appearance.
     * @return the current instance of RequestBuilder for method chaining.
     * @throws IllegalArgumentException if the number of placeholders does not match the number of provided values,
     *                                  or if no placeholders are found in the `serviceName`.
     */
    public RequestBuilder setPathParameters(String... values) {
        // Check if serviceName contains any placeholders
        if (!serviceName.contains("{") || !serviceName.contains("}")) {
            throw new IllegalArgumentException(
                    "No placeholders found in the serviceName: " + serviceName + ". Cannot set path parameters."
            );
        }

        String[] placeholders = serviceName.split("\\{");
        int placeholderCount = placeholders.length - 1; // Count of placeholders
        if (placeholderCount != values.length) {
            throw new IllegalArgumentException(
                    "Number of provided values (" + values.length + ") does not match the number of placeholders (" +
                            placeholderCount + ") in the serviceName: " + serviceName
            );
        }

        for (int i = 1; i < placeholders.length; i++) {
            String placeholder = placeholders[i].split("}")[0]; // Extract placeholder name
            serviceName = serviceName.replace("{" + placeholder + "}", values[i - 1]);
        }
        return this;
    }

    /**
     * @param parameters     a list of key/value pairs that will be sent as parameters with this API call, is nullable, Example: Arrays.asList(Arrays.asList("itemId", "123"), Arrays.asList("contents", XMLContents));
     * @param parametersType FORM, QUERY
     * @return a self-reference to be used to continue building your API request
     * @deprecated Use setParameters(Map<String, Object>, RestActions.ParametersType) instead.
     * This method will be removed in a future release.
     * Sets the parameters (if any) for the API request that you're currently building. A request usually has only one of the following: urlArguments, parameters+type, or body
     */
    @Deprecated
    public RequestBuilder setParameters(List<List<Object>> parameters, RestActions.ParametersType parametersType) {
        this.parameters = parameters;
        this.parametersType = parametersType;
        return this;
    }

    /**
     * Sets API request parameters using a Map<String, Object>.
     * Recommended for better readability and usability.
     *
     * @param parameters     A Map where keys represent parameter names, and values represent corresponding values.
     * @param parametersType The type of parameters (FORM, QUERY).
     * @return A self-reference to continue building the API request.
     */
    public RequestBuilder setParameters(Map<String, Object> parameters, RestActions.ParametersType parametersType) {
        this.parametersMap = parameters;  // Store in new field
        this.parametersType = parametersType;
        return this;
    }

    /**
     * Sets the body (if any) for the API request that you're currently building. A request usually has only one of the following: urlArguments, parameters+type, or body
     *
     * @param requestBody Specify an Object request content that will automatically be serialized to JSON or XML and sent with the request. If the object is a primitive or Number the object will be converted to a String and put in the request body. This works for the POST, PUT and PATCH methods only. Trying to do this for the other http methods will cause an exception to be thrown.
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setRequestBody(Object requestBody) {
        this.requestBody = requestBody;
        return this;
    }

    /**
     * Sets the body (if any) for the API request that you're currently building. A request usually has only one of the following: urlArguments, parameters+type, or body
     *
     * @param relativeFilePath Specify the path to a file that will be used as an Object request content that will automatically be serialized to JSON or XML and sent with the request. If the object is a primitive or Number the object will be converted to a String and put in the request body. This works for the POST, PUT and PATCH methods only. Trying to do this for the other http methods will cause an exception to be thrown.
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setRequestBodyFromFile(String relativeFilePath) {
        this.requestBody = new FileActions().readFile(relativeFilePath);
        return this;
    }

    /**
     * Sets the content type for the API request that you're currently building. By default, this value is set to `ContentType.ANY` but you can change it by calling this method.
     *
     * @param contentType Enumeration of common IANA content-types. This may be used to specify a request or response content-type more easily than specifying the full string each time. Example: ContentType.ANY
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setContentType(ContentType contentType) {
        this.contentType = contentType;
        return this;
    }

    /**
     * Sets the content type for the API request that you're currently building. By default, this value is set to `ContentType.ANY` but you can change it by calling this method.
     *
     * @param contentType String value representing IANA content-type.
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setContentType(String contentType) {
        this.contentType = ContentType.fromContentType(contentType);
        return this;
    }

    /**
     * Tells whether REST Assured should automatically append the content charset to the content-type header if not defined explicitly.
     * Note that this does not affect multipart form data.
     * Default is true.
     *
     * @param appendDefaultContentCharsetToContentTypeIfUndefined Whether REST Assured should automatically append the content charset to the content-type header if not defined explicitly.
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder appendDefaultContentCharsetToContentTypeIfUndefined(boolean appendDefaultContentCharsetToContentTypeIfUndefined) {
        this.appendDefaultContentCharsetToContentTypeIfUndefined = appendDefaultContentCharsetToContentTypeIfUndefined;
        return this;
    }

    /**
     * Tells whether REST Assured should automatically encode the URI if not defined explicitly.
     * Note that this does not affect multipart form data.
     * Default is true.
     *
     * @param urlEncodingEnabled Whether REST Assured should automatically encode the URI if not defined explicitly.
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder enableUrlEncoding(boolean urlEncodingEnabled) {
        this.urlEncodingEnabled = urlEncodingEnabled;
        return this;
    }

    /**
     * Append a header to the current session to be used in the current and all the following requests. This feature is commonly used for authentication tokens.
     *
     * @param key   the name of the header that you want to add
     * @param value the value that will be put inside the key
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder addHeader(String key, String value) {
        this.sessionHeaders.put(key, value);
        return this;
    }

    /**
     * Append headers to the current session to be used in the current and all the following requests. This feature is commonly used for authentication tokens.
     *
     * @param headers a map of headers that you want to add
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder addHeaders(Map<String, String> headers) {
        this.sessionHeaders.putAll(headers);
        return this;
    }

    /**
     * Append a cookie to the current session to be used in the current and all the following requests. This feature is commonly used for authentication cookies.
     *
     * @param key   the name of the cookie that you want to add
     * @param value the value that will be put inside the key
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder addCookie(String key, Object value) {
        this.sessionCookies.put(key, value);
        return this;
    }

    /**
     * Append cookies to the current session to be used in the current and all the following requests. This feature is commonly used for authentication cookies.
     *
     * @param cookies a map of cookies that you want to add
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder addCookies(Map<String, String> cookies) {
        this.sessionCookies.putAll(cookies);
        return this;
    }

    /**
     * Set the authentication method that will be used by the API request that you're currently building. By default, this value is set to `AuthenticationType.NONE` but you can change it by calling this method. If you use this method the authentication token will be saved automatically for all the following requests using the same session.
     *
     * @param username           the value of the username that you will be using to authenticate the current API request.
     * @param password           the value of the password that you will be using to authenticate the current API request.
     * @param authenticationType the type of your authentication method {BASIC, FORM, NONE}
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setAuthentication(String username, String password, AuthenticationType authenticationType) {
        this.authenticationType = authenticationType;
        this.authenticationUsername = username;
        this.authenticationPassword = password;
        return this;
    }

    /**
     * After you finish building your request, use this method to trigger the request and get back the response object.
     *
     * @return Response; returns the full response object for further manipulation
     */
    public Response perform() {
        return performRequest();
    }

    /**
     * After you finish building your request, use this method to trigger the request and get back the response object.
     *
     * @return Response; returns the full response object for further manipulation
     */
    @Step("Perform {this.requestType} request to {this.serviceURI}{this.serviceName}")
    public Response performRequest() {
        String request = prepareRequestURLWithParameters();
        RequestSpecification specs = prepareRequestSpecifications();

        setupAuthentication(specs);

        long startTime = System.currentTimeMillis();
        Response response = null;
        try {
            response = sendRequest(request, specs);
            long endTime = System.currentTimeMillis();
            if (SHAFT.Properties.performance.isEnablePerformanceReport()) {
                double responseTime = endTime - startTime;
                String normalizedEndpoint = normalizeEndpoint(serviceName);
                logResponseTime(normalizedEndpoint, responseTime);
            }

            handleResponse(response, specs);
        } catch (Exception e) {
            handleException(request, specs, response, e);
        }

        session.setLastResponse(response);
        return response;
    }

    private String normalizeEndpoint(String endpoint) {
        // Simplified normalization logic to remove digits and trailing slashes
        return endpoint.replaceAll("/\\d+", "").replaceAll("/$", "");
    }

    private synchronized void logResponseTime(String endpoint, double responseTime) {
        performanceData.computeIfAbsent(endpoint, k -> new ArrayList<>()).add(responseTime);
    }

    private String prepareRequestURLWithParameters() {
        String request = session.prepareRequestURL(serviceURI, urlArguments, serviceName);
        if (parametersType == RestActions.ParametersType.QUERY) {
            if (parametersMap != null) {
                request = addParametersToUrl(request, parametersMap);
            } else if (parameters != null) {
                request = addParametersToUrl(request, parameters);
            }
        }
        return request;
    }

    private RequestSpecification prepareRequestSpecifications() {
        List<List<Object>> paramsToSend = this.parameters;

        if ((paramsToSend == null || paramsToSend.isEmpty())
                && this.parametersMap != null && !this.parametersMap.isEmpty()) {
            paramsToSend = new ArrayList<>();
            for (Map.Entry<String, Object> e : this.parametersMap.entrySet()) {
                // Preserve exact object type (File stays File, etc.)
                paramsToSend.add(Arrays.asList(e.getKey(), e.getValue()));
            }
        }
        return session.prepareRequestSpecs(paramsToSend, parametersType, requestBody, contentType, sessionCookies, sessionHeaders, sessionConfig, appendDefaultContentCharsetToContentTypeIfUndefined, urlEncodingEnabled);
    }

    private void setupAuthentication(RequestSpecification specs) {
        switch (authenticationType) {
            case BASIC -> specs.auth().preemptive().basic(authenticationUsername, authenticationPassword);
            case FORM -> specs.auth().form(authenticationUsername, authenticationPassword);
            case NONE -> {
            }  // Do nothing
        }
    }

    private Response sendRequest(String request, RequestSpecification specs) {
        if (!isSupportedRequestType()) {
            RestActions.failAction(request);
            return null;
        }
        return session.sendRequest(requestType, request, specs);
    }

    private void handleResponse(Response response, RequestSpecification specs) {
        boolean responseStatus = session.evaluateResponseStatusCode(Objects.requireNonNull(response), targetStatusCode);
        String reportMessage = session.prepareReportMessage(response, targetStatusCode, requestType, serviceName, contentType, urlArguments);
        if (!Boolean.TRUE.equals(responseStatus)) {
            throw new AssertionError("Invalid response status code; Expected " + targetStatusCode + " but found " + response.getStatusCode() + ".");
        }

        if (!reportMessage.isEmpty()) {
            RestActions.passAction(reportMessage, requestBody, specs, response);
        } else {
            RestActions.failAction(reportMessage, requestBody, specs, response);
        }
    }

    private void handleException(String request, RequestSpecification specs, Response response, Exception e) {
        if (response != null) {
            RestActions.failAction(request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", requestBody, specs, response, e);
        } else {
            RestActions.failAction(request, e);
        }
    }

    private boolean isSupportedRequestType() {
        return requestType == RestActions.RequestType.POST ||
                requestType == RestActions.RequestType.PATCH ||
                requestType == RestActions.RequestType.PUT ||
                requestType == RestActions.RequestType.GET ||
                requestType == RestActions.RequestType.DELETE;
    }

    private String addParametersToUrl(String url, Map<String, Object> parameters) {
        StringBuilder urlWithParams = new StringBuilder(url);

        if (!url.contains("?")) {
            urlWithParams.append("?");
        } else {
            urlWithParams.append("&");
        }

        for (Map.Entry<String, Object> param : parameters.entrySet()) {
            urlWithParams.append(param.getKey()).append("=").append(param.getValue()).append("&");
        }

        // Remove the last '&' if parameters exist
        if (!parameters.isEmpty()) {
            urlWithParams.setLength(urlWithParams.length() - 1);
        }

        return urlWithParams.toString();
    }


    private String addParametersToUrl(String url, List<List<Object>> parameters) {
        StringBuilder urlWithParams = new StringBuilder(url);
        if (!url.contains("?")) {
            urlWithParams.append("?");
        } else {
            urlWithParams.append("&");
        }
        for (List<Object> param : parameters) {
            urlWithParams.append(param.get(0)).append("=").append(param.get(1)).append("&");
        }
        // Remove the last '&'
        urlWithParams.setLength(urlWithParams.length() - 1);
        return urlWithParams.toString();
    }

    /**
     * The type of your authentication method {BASIC, FORM, NONE}
     */
    public enum AuthenticationType {
        BASIC, FORM, NONE
    }
}