package com.shaft.gui.driver;

/** Browser-context permission controls with explicit origin scoping. */
public interface PermissionActionsContract {
    /** @return the owning browser facade */
    BrowserActionsContract and();

    /**
     * Grants permissions for every origin in the active Playwright context.
     * Selenium requires an origin and therefore fails explicitly; use {@link #grantFor(String, String...)} there.
     *
     * @param permissions nonblank native browser permission names
     * @return this facade
     * @throws UnsupportedOperationException when the live backend cannot perform a global grant
     */
    PermissionActionsContract grant(String... permissions);

    /**
     * Grants permissions for one credential-free origin such as {@code https://example.test:8443}.
     *
     * @param origin scheme, host, and optional valid port; paths, credentials, query, and fragment are rejected
     * @param permissions nonblank native browser permission names
     * @return this facade
     */
    PermissionActionsContract grantFor(String origin, String... permissions);

    /**
     * Denies permissions for one origin through Selenium WebDriver BiDi. Playwright fails explicitly because its
     * live-context API provides grant and clear, not explicit denial.
     */
    PermissionActionsContract denyFor(String origin, String... permissions);

    /**
     * Restores permissions to prompt through Selenium WebDriver BiDi. Playwright callers use {@link #clear()}.
     */
    PermissionActionsContract promptFor(String origin, String... permissions);

    /**
     * Clears every Playwright context grant, or restores Selenium permissions changed through any SHAFT browser
     * facade for the same live driver to prompt. Repeating this operation is safe.
     */
    PermissionActionsContract clear();
}
