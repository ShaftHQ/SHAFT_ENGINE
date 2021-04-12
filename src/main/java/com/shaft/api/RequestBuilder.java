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
    private int targetStatusCode = 200;

    private String urlArguments = null;
    private List<List<Object>> parameters = null;
    private RestActions.ParametersType parametersType = null;
    private Object requestBody = null;
    private ContentType contentType = ContentType.ANY;

    //    private RequestSpecBuilder specBuilder;
    private AuthenticationType authenticationType;
    private String authenticationUsername;
    private String authenticationPassword;

    //TODO: Document everything
    RequestBuilder(RestActions session, String serviceName, RestActions.RequestType requestType) {
        initializeVariables(session, serviceName, requestType);
    }

    RequestBuilder(String serviceURI, String serviceName, RestActions.RequestType requestType) {
        initializeVariables(new RestActions(serviceURI), serviceName, requestType);
    }

    private void initializeVariables(RestActions session, String serviceName, RestActions.RequestType requestType) {
        this.session = session;
        this.serviceURI = session.getServiceURI();
        this.sessionCookies = session.getSessionCookies();
        this.sessionHeaders = session.getSessionHeaders();
        this.requestType = requestType;
        this.serviceName = serviceName;
//        this.specBuilder = new RequestSpecBuilder();
    }

    public RequestBuilder setTargetStatusCode(int targetStatusCode) {
        this.targetStatusCode = targetStatusCode;
        return this;
    }

    public RequestBuilder setUrlArguments(String urlArguments) {
        this.urlArguments = urlArguments;
        return this;
    }

    public RequestBuilder setParameters(List<List<Object>> parameters) {
        this.parameters = parameters;
        return this;
    }

    public RequestBuilder setParametersType(RestActions.ParametersType parametersType) {
        this.parametersType = parametersType;
        return this;
    }

    public RequestBuilder setRequestBody(Object requestBody) {
        this.requestBody = requestBody;
        return this;
    }

    public RequestBuilder setContentType(ContentType contentType) {
        this.contentType = contentType;
        return this;
    }

    public RequestBuilder addHeader(String key, String value) {
        this.sessionHeaders.put(key, value);
        return this;
    }

    public RequestBuilder addCookie(String key, Object value) {
        this.sessionCookies.put(key, value);
        return this;
    }

    public RequestBuilder setAuthentication(String username, String password, AuthenticationType authenticationType) {
        this.authenticationType = authenticationType;
        this.authenticationUsername = username;
        this.authenticationPassword = password;
        return this;
    }

    public Response performRequest() {
        String request = session.prepareRequestURL(serviceURI, urlArguments, serviceName);
        RequestSpecification specs = session.prepareRequestSpecs(parameters, parametersType, requestBody, contentType, sessionCookies, sessionHeaders);

        switch (this.authenticationType) {
            case BASIC -> specs.auth().preemptive().basic(this.authenticationUsername, this.authenticationPassword);
            case FORM -> specs.auth().form(this.authenticationUsername, this.authenticationPassword);
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
                RestActions.passAction(reportMessage, requestBody, response);
            } else {
                RestActions.failAction(reportMessage, requestBody, response);
            }
        } catch (Exception rootCauseException) {
            ReportManagerHelper.log(rootCauseException);
            if (response != null) {
                RestActions.failAction(request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", requestBody,
                        response, rootCauseException);
            } else {
                RestActions.failAction(request, rootCauseException);
            }
        }
        return response;
    }

    public enum AuthenticationType {
        BASIC, FORM
    }
}
