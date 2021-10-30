package com.shaft.api;

import com.shaft.tools.io.ReportManagerHelper;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class RequestBuilder {
    private RestActions session;
    private Map<String, String> sessionHeaders;
    private Map<String, Object> sessionCookies;
    private RestActions.RequestType requestType;
    private String serviceName;
    private String serviceURI;
    private int targetStatusCode;

    private String urlArguments = null;
    private List<List<Object>> parameters = null;
    private RestActions.ParametersType parametersType = null;
    private Object requestBody = null;
    private String contentType = null;

    private AuthenticationType authenticationType;
    private String authenticationUsername;
    private String authenticationPassword;

    private boolean appendDefaultContentCharsetToContentTypeIfUndefined;

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
        this.requestType = requestType;
        this.serviceName = serviceName;
        this.targetStatusCode = 200;
        this.contentType = ContentType.ANY.toString();
        this.authenticationType = AuthenticationType.NONE;
        this.appendDefaultContentCharsetToContentTypeIfUndefined = true;
    }

    /**
     * Sets the expected target status code for the API request that you're currently building. By default this value is set to 200 but you can change it by calling this method.
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
     * Sets the parameters (if any) for the API request that you're currently building. A request usually has only one of the following: urlArguments, parameters+type, or body
     *
     * @param parameters a list of key/value pairs that will be sent as parameters with this API call, is nullable, Example: Arrays.asList(Arrays.asList("itemId", "123"), Arrays.asList("contents", XMLcontents));
     * @param parametersType FORM, QUERY
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setParameters(List<List<Object>> parameters, RestActions.ParametersType parametersType) {
        this.parameters = parameters;
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
     * Sets the content type for the API request that you're currently building. By default this value is set to ContentType.ANY but you can change it by calling this method.
     *
     * @param contentType Enumeration of common IANA content-types. This may be used to specify a request or response content-type more easily than specifying the full string each time. Example: ContentType.ANY
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setContentType(ContentType contentType) {
        this.contentType = contentType.toString();
        return this;
    }

    /**
     * Sets the content type for the API request that you're currently building. By default this value is set to ContentType.ANY but you can change it by calling this method.
     *
     * @param contentType String value representing IANA content-type.
     * @return a self-reference to be used to continue building your API request
     */
    public RequestBuilder setContentType(String contentType) {
        this.contentType = contentType;
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
     * Set the authentication method that will be used by the API request that you're currently building. By default this value is set to AuthenticationType.NONE but you can change it by calling this method. If you use thie method the authentication token will be saved automatically for all the following requests using the same session.
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
    public Response performRequest() {
        String request = session.prepareRequestURL(serviceURI, urlArguments, serviceName);
        RequestSpecification specs = session.prepareRequestSpecs(parameters, parametersType, requestBody, contentType, sessionCookies, sessionHeaders, appendDefaultContentCharsetToContentTypeIfUndefined);

        switch (this.authenticationType) {
            case BASIC -> specs.auth().preemptive().basic(this.authenticationUsername, this.authenticationPassword);
            case FORM -> specs.auth().form(this.authenticationUsername, this.authenticationPassword);
            case NONE -> {
            } //do nothing
        }

        Response response = null;
        try {
            if (requestType.equals(RestActions.RequestType.POST) || requestType.equals(RestActions.RequestType.PATCH)
                    || requestType.equals(RestActions.RequestType.PUT) || requestType.equals(RestActions.RequestType.GET)
                    || requestType.equals(RestActions.RequestType.DELETE)) {
                response = session.sendRequest(requestType, request, specs);
            } else {
                RestActions.failAction(request);
            }

            boolean responseStatus = session.evaluateResponseStatusCode(Objects.requireNonNull(response), targetStatusCode);
            String reportMessage = session.prepareReportMessage(response, targetStatusCode, requestType, serviceName,
                    contentType, urlArguments);
            if (!"".equals(reportMessage) && Boolean.TRUE.equals(responseStatus)) {
                RestActions.passAction(reportMessage, requestBody, specs, response);
            } else {
                RestActions.failAction(reportMessage, requestBody, specs, response);
            }
        } catch (Exception rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            if (response != null) {
                RestActions.failAction(request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", requestBody,specs,
                        response, rootCauseException);
            } else {
                RestActions.failAction(request, rootCauseException);
            }
        }
        return response;
    }

    /**
     * The type of your authentication method {BASIC, FORM, NONE}
     */
    public enum AuthenticationType {
        BASIC, FORM, NONE
    }
}
