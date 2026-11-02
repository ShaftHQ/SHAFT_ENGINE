package com.shaft.gui.driver;

import com.shaft.gui.browser.NetworkInterceptionRequestBuilder;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.function.Predicate;

/**
 * Cohesive browser-network actions shared by Selenium/Appium and Playwright facades.
 * Implementations retain their native backend behavior and fail explicitly when the live
 * session does not support the requested operation.
 */
public interface NetworkActionsContract {
    /** @return the owning browser facade */
    BrowserActionsContract and();

    /**
     * Mocks matching requests with the supplied response.
     * @throws UnsupportedOperationException when interception is unavailable for the live session
     */
    NetworkActionsContract mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    /**
     * Starts a fluent request-interception rule.
     * @throws UnsupportedOperationException when interception is unavailable for the live session
     */
    NetworkInterceptionRequestBuilder<?> interceptRequest();

    /**
     * Intercepts matching requests with the supplied response.
     * @throws UnsupportedOperationException when interception is unavailable for the live session
     */
    NetworkActionsContract intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    /**
     * Clears rules owned by this SHAFT session.
     * @throws UnsupportedOperationException when interception is unavailable for the live session
     */
    NetworkActionsContract clear();

    /**
     * Records matching HTTP exchanges to a contract file.
     * @throws UnsupportedOperationException when observation is unavailable for the live session
     */
    NetworkActionsContract startContractRecording(String contractFilePath, String... urlContains);

    /**
     * Fails immediately when matching HTTP exchanges differ from the contract.
     * @throws UnsupportedOperationException when observation is unavailable for the live session
     */
    NetworkActionsContract assertContract(String contractFilePath, String... urlContains);

    /**
     * Collects matching HTTP contract differences as soft verification failures.
     * @throws UnsupportedOperationException when observation is unavailable for the live session
     */
    NetworkActionsContract verifyContract(String contractFilePath, String... urlContains);

    /**
     * Replays responses from a SHAFT HTTP contract.
     * @throws UnsupportedOperationException when interception is unavailable for the live session
     */
    NetworkActionsContract replayContract(String contractFilePath);

    /**
     * Replays responses from a HAR file.
     * @throws UnsupportedOperationException when interception is unavailable for the live session
     */
    NetworkActionsContract routeFromHar(String harFilePath);

    /**
     * Emulates a fully offline browser context.
     * @throws UnsupportedOperationException when offline emulation is unavailable for the live session
     */
    NetworkActionsContract offline();

    /**
     * Restores normal network connectivity after SHAFT offline/throttle emulation.
     * @throws UnsupportedOperationException when network emulation is unavailable for the live session
     */
    NetworkActionsContract online();

    /**
     * Applies latency in milliseconds and throughput limits in kilobits per second.
     * @throws IllegalArgumentException when any value is negative
     * @throws UnsupportedOperationException when throttling is unavailable for the live session
     */
    NetworkActionsContract throttle(long latencyMs, long downloadKbps, long uploadKbps);

    /**
     * Blocks requests matching backend-native URL patterns.
     * @throws UnsupportedOperationException when resource blocking is unavailable for the live session
     */
    NetworkActionsContract block(String... urlPatterns);
}
