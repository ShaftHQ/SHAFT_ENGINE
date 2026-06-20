package com.shaft.gui.browser.internal;

import io.restassured.response.Response;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Describes one browser network interception rule.
 */
public final class BrowserNetworkInterceptionRule {
    private final Predicate<HttpRequest> requestPredicate;
    private final Function<HttpRequest, HttpResponse> responseFactory;
    private final Consumer<Response> responseValidation;

    private BrowserNetworkInterceptionRule(Predicate<HttpRequest> requestPredicate,
                                           Function<HttpRequest, HttpResponse> responseFactory,
                                           Consumer<Response> responseValidation) {
        this.requestPredicate = requestPredicate;
        this.responseFactory = responseFactory;
        this.responseValidation = responseValidation;
    }

    /**
     * Creates a rule that returns a mocked response without sending the real request.
     *
     * @param requestPredicate predicate used to match outgoing browser requests
     * @param responseFactory  factory used to build the mocked response
     * @return the interception rule
     */
    public static BrowserNetworkInterceptionRule mock(Predicate<HttpRequest> requestPredicate,
                                                      Function<HttpRequest, HttpResponse> responseFactory) {
        return new BrowserNetworkInterceptionRule(requestPredicate, responseFactory, null);
    }

    /**
     * Creates a rule that lets the request proceed and validates the real response.
     *
     * @param requestPredicate   predicate used to match outgoing browser requests
     * @param responseValidation validation callback for the real response
     * @return the interception rule
     */
    public static BrowserNetworkInterceptionRule validate(Predicate<HttpRequest> requestPredicate,
                                                          Consumer<Response> responseValidation) {
        return new BrowserNetworkInterceptionRule(requestPredicate, null, responseValidation);
    }

    boolean matches(HttpRequest request) {
        return requestPredicate.test(request);
    }

    boolean mocksResponse() {
        return responseFactory != null;
    }

    HttpResponse createResponse(HttpRequest request) {
        return responseFactory.apply(request);
    }

    void validate(Response response) {
        if (responseValidation != null) {
            responseValidation.accept(response);
        }
    }
}
