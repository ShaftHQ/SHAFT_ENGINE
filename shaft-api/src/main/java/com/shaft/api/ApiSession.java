package com.shaft.api;

import com.shaft.api.validation.internal.RestValidationsBuilder;
import com.shaft.validation.ValidationEnums;
import io.restassured.response.Response;

import java.util.List;

/**
 * Session context for REST/HTTP API testing.
 * {@code SHAFT.API} in shaft-engine extends this class for backward compatibility.
 */
@SuppressWarnings("unused")
public class ApiSession {
    protected final RestActions session;

    /**
     * Creates a new API session pointing at the given base service URI.
     *
     * @param serviceURI the base URI of the target web service
     */
    public ApiSession(String serviceURI) {
        session = new RestActions(serviceURI, this);
    }

    /**
     * Wraps an existing {@link RestActions} session for continued use.
     *
     * @param existingSession an already-initialised REST session
     */
    public ApiSession(RestActions existingSession) {
        session = existingSession;
    }

    /**
     * Builds a GET request for the specified service endpoint.
     */
    public RequestBuilder get(String serviceName) {
        return session.buildNewRequest(serviceName, RestActions.RequestType.GET);
    }

    /**
     * Builds a POST request for the specified service endpoint.
     */
    public RequestBuilder post(String serviceName) {
        return session.buildNewRequest(serviceName, RestActions.RequestType.POST);
    }

    /**
     * Builds a PATCH request for the specified service endpoint.
     */
    public RequestBuilder patch(String serviceName) {
        return session.buildNewRequest(serviceName, RestActions.RequestType.PATCH);
    }

    /**
     * Builds a DELETE request for the specified service endpoint.
     */
    public RequestBuilder delete(String serviceName) {
        return session.buildNewRequest(serviceName, RestActions.RequestType.DELETE);
    }

    /**
     * Builds a PUT request for the specified service endpoint.
     */
    public RequestBuilder put(String serviceName) {
        return session.buildNewRequest(serviceName, RestActions.RequestType.PUT);
    }

    /**
     * Adds a persistent header that will be sent with every subsequent request in this session.
     */
    public void addHeader(String key, String value) {
        session.addHeaderVariable(key, value);
    }

    /**
     * Adds a persistent cookie that will be sent with every subsequent request in this session.
     */
    public void addCookie(String key, String value) {
        session.addCookieVariable(key, value);
    }

    /**
     * Starts building a hard assertion against the last API response.
     *
     * @return a {@link RestValidationsBuilder} for response assertions
     */
    public RestValidationsBuilder assertThatResponse() {
        return new RestValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, session.getResponse());
    }

    /**
     * Starts building a soft verification against the last API response.
     *
     * @return a {@link RestValidationsBuilder} for response verifications
     */
    public RestValidationsBuilder verifyThatResponse() {
        return new RestValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, session.getResponse());
    }

    /**
     * Returns the raw REST Assured {@link Response} from the most recent request.
     */
    public Response getResponse() {
        return session.getResponse();
    }

    /**
     * Returns the response body of the most recent request as a string.
     */
    public String getResponseBody() {
        return RestActions.getResponseBody(session.getResponse());
    }

    /**
     * Returns the HTTP status code of the most recent response.
     */
    public int getResponseStatusCode() {
        return RestActions.getResponseStatusCode(session.getResponse());
    }

    /**
     * Returns the response time in milliseconds for the most recent request.
     */
    public long getResponseTime() {
        return RestActions.getResponseTime(session.getResponse());
    }

    /**
     * Extracts a value from the most recent JSON response using a JSONPath expression.
     *
     * @param jsonPath the JSONPath expression (e.g., {@code "$.data.id"})
     * @return the extracted value as a string
     */
    public String getResponseJSONValue(String jsonPath) {
        return RestActions.getResponseJSONValue(session.getResponse(), jsonPath);
    }

    /**
     * Extracts a list of values from the most recent JSON response using a JSONPath expression.
     *
     * @param jsonPath the JSONPath expression
     * @return the extracted values as a list of objects
     */
    public List<Object> getResponseJSONValueAsList(String jsonPath) {
        return RestActions.getResponseJSONValueAsList(session.getResponse(), jsonPath);
    }

    /**
     * Extracts a value from the most recent XML response using an XPath expression.
     *
     * @param xmlPath the XPath expression
     * @return the extracted value as a string
     */
    public String getResponseXMLValue(String xmlPath) {
        return RestActions.getResponseXMLValue(session.getResponse(), xmlPath);
    }

    /**
     * Extracts a list of values from the most recent XML response using an XPath expression.
     *
     * @param xmlPath the XPath expression
     * @return the extracted values as a list of objects
     */
    public List<Object> getResponseXMLValueAsList(String xmlPath) {
        return RestActions.getResponseXMLValueAsList(session.getResponse(), xmlPath);
    }
}
