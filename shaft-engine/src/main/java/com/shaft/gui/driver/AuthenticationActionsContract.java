package com.shaft.gui.driver;

/** Session-scoped HTTP Basic authentication policies and navigation convenience. */
public interface AuthenticationActionsContract {
    /** @return the owning browser facade */
    BrowserActionsContract and();

    /**
     * Registers credentials for the current page origin. Use {@link #basicFor} before the first navigation.
     *
     * @param username HTTP Basic username; empty is allowed, {@code null} and colon are rejected
     * @param password HTTP Basic password; empty is allowed, {@code null} is rejected
     * @return this facade
     */
    AuthenticationActionsContract basic(String username, String password);

    /**
     * Registers credentials for one normalized, credential-free origin.
     *
     * @param origin scheme, host, and optional valid port
     * @param username HTTP Basic username
     * @param password HTTP Basic password
     * @return this facade
     */
    AuthenticationActionsContract basicFor(String origin, String username, String password);

    /**
     * Registers origin-scoped credentials and navigates without embedding them in the URL.
     *
     * @return the owning browser facade after navigation
     */
    BrowserActionsContract navigateTo(String url, String username, String password);

    /**
     * Clears SHAFT-managed Playwright policies. Selenium fails explicitly because its pinned
     * {@code HasAuthentication} API has no unregister operation; recreate that driver to clear handlers.
     */
    AuthenticationActionsContract clear();
}
