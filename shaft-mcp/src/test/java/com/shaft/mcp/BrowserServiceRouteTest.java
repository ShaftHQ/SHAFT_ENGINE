package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.Map;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Focused, driver-free coverage for the request predicate and mocked-response building that back
 * the {@code browser_route}/{@code browser_unroute} MCP tools.
 */
class BrowserServiceRouteTest {
    @Test
    void routePredicateRequiresUrlGlobOrUrl() {
        assertThrows(IllegalArgumentException.class, () -> BrowserService.routePredicate("GET", "", ""));
    }

    @Test
    void routePredicateRejectsUnsupportedMethod() {
        assertThrows(IllegalArgumentException.class, () -> BrowserService.routePredicate("NOPE", "**/api/**", ""));
    }

    @Test
    void routePredicateMatchesGlobAndMethod() {
        Predicate<HttpRequest> predicate = BrowserService.routePredicate("GET", "**/api/users**", "");

        assertTrue(predicate.test(new HttpRequest(HttpMethod.GET, "https://example.test/api/users?id=1")));
        assertFalse(predicate.test(new HttpRequest(HttpMethod.POST, "https://example.test/api/users?id=1")));
        assertFalse(predicate.test(new HttpRequest(HttpMethod.GET, "https://example.test/api/other")));
    }

    @Test
    void routePredicateMatchesAnyMethodWhenMethodBlank() {
        Predicate<HttpRequest> predicate = BrowserService.routePredicate("", "**/api/users**", "");

        assertTrue(predicate.test(new HttpRequest(HttpMethod.GET, "https://example.test/api/users")));
        assertTrue(predicate.test(new HttpRequest(HttpMethod.POST, "https://example.test/api/users")));
    }

    @Test
    void routePredicateMatchesExactUrlWhenGlobBlank() {
        Predicate<HttpRequest> predicate = BrowserService.routePredicate("", "", "https://example.test/api/users");

        assertTrue(predicate.test(new HttpRequest(HttpMethod.GET, "https://example.test/api/users")));
        assertFalse(predicate.test(new HttpRequest(HttpMethod.GET, "https://example.test/api/users?id=1")));
    }

    @Test
    void routePredicatePrefersUrlGlobOverExactUrlWhenBothSet() {
        Predicate<HttpRequest> predicate = BrowserService.routePredicate("", "**/api/**", "https://example.test/other");

        assertTrue(predicate.test(new HttpRequest(HttpMethod.GET, "https://example.test/api/users")));
        assertFalse(predicate.test(new HttpRequest(HttpMethod.GET, "https://example.test/other")));
    }

    @Test
    void buildMockResponseDefaultsStatusTo200() {
        HttpResponse response = BrowserService.buildMockResponse(0, "{}", Map.of("X-Test", "1"));

        assertEquals(200, response.getStatus());
        assertEquals("1", response.getHeader("X-Test"));
    }

    @Test
    void buildMockResponseHonorsExplicitStatusAndOmitsBodyWhenNull() {
        HttpResponse response = BrowserService.buildMockResponse(503, null, null);

        assertEquals(503, response.getStatus());
        assertNull(response.getHeader("X-Test"));
    }
}
