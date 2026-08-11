package com.shaft.gui.browser;

import com.shaft.gui.driver.NetworkActionsContract;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.Objects;
import java.util.function.Predicate;

/** Discoverable Selenium/Appium browser-network actions. */
public final class NetworkActions implements NetworkActionsContract {
    private final BrowserActions browser;

    NetworkActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override
    public BrowserActions and() {
        return browser;
    }

    @Override
    public NetworkActions mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        perform("mock", () -> browser.mock(requestPredicate, mockedResponse));
        return this;
    }

    @Override
    public NetworkInterceptionRequestBuilder<BrowserActions> interceptRequest() {
        return browser.networkInterceptRequest();
    }

    @Override
    public NetworkActions intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        perform("intercept", () -> browser.intercept(requestPredicate, mockedResponse));
        return this;
    }

    @Override
    public NetworkActions clear() {
        perform("clear", browser::clearNetworkInterceptors);
        return this;
    }

    @Override
    public NetworkActions startContractRecording(String contractFilePath, String... urlContains) {
        perform("start-contract-recording", () -> browser.startNetworkContractRecording(contractFilePath, urlContains));
        return this;
    }

    @Override
    public NetworkActions assertContract(String contractFilePath, String... urlContains) {
        perform("assert-contract", () -> browser.startNetworkContractAssertion(contractFilePath, urlContains));
        return this;
    }

    @Override
    public NetworkActions verifyContract(String contractFilePath, String... urlContains) {
        perform("verify-contract", () -> browser.startNetworkContractVerification(contractFilePath, urlContains));
        return this;
    }

    @Override
    public NetworkActions replayContract(String contractFilePath) {
        perform("replay-contract", () -> browser.replayContract(contractFilePath));
        return this;
    }

    @Override
    public NetworkActions routeFromHar(String harFilePath) {
        perform("route-from-har", () -> browser.routeFromHar(harFilePath));
        return this;
    }

    @Override
    public NetworkActions offline() {
        perform("offline", browser::goOffline);
        return this;
    }

    @Override
    public NetworkActions online() {
        perform("online", browser::restoreNetwork);
        return this;
    }

    @Override
    public NetworkActions throttle(long latencyMs, long downloadKbps, long uploadKbps) {
        perform("throttling", () -> browser.throttleNetwork(latencyMs, downloadKbps, uploadKbps));
        return this;
    }

    @Override
    public NetworkActions block(String... urlPatterns) {
        perform("resource-blocking", () -> browser.blockNetworkResources(urlPatterns));
        return this;
    }

    private void perform(String operation, Runnable action) {
        browser.performNetworkAction(operation, action);
    }
}
