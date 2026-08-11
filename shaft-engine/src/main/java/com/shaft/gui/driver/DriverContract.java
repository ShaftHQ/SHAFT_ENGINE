package com.shaft.gui.driver;

import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.element.TouchActions;

/**
 * Public contract for SHAFT GUI automation sessions.
 */
public interface DriverContract {

    /**
     * Returns an immutable snapshot of the automation features available to this session.
     *
     * <p>The default is deliberately fail-closed so existing implementations gain no unsupported
     * behavior. As with any Java interface method addition, an implementation that also inherits
     * an unrelated same-signature method with an incompatible return type must resolve that source
     * conflict explicitly. Competing unrelated default implementations also require an explicit
     * override even when their return types match.</p>
     *
     * @return effective capability snapshot
     */
    default AutomationCapabilities capabilities() {
        return AutomationCapabilities.unknown(
                "This GUI driver implementation did not declare its automation capabilities.");
    }

    /**
     * Returns the primary native automation object for the active backend.
     *
     * @return the live backend driver object
     */
    Object getDriver();

    /**
     * Alias for {@link #getDriver()} that reads naturally when callers handle
     * the session through the generic GUI driver contract.
     *
     * @return the live backend driver object
     */
    default Object getNativeDriver() {
        return getDriver();
    }

    /**
     * Returns the native backend context object when the backend exposes one.
     *
     * @return the active backend context
     */
    default Object getNativeContext() {
        return getNativeDriver();
    }

    /**
     * Returns browser-level actions and context helpers.
     *
     * @return browser actions facade
     */
    BrowserActionsContract browser();

    /**
     * Returns element-level actions and assertions.
     *
     * @return element actions facade
     */
    ElementActionsContract element();

    /**
     * Returns touch-action helpers for mobile scenarios.
     *
     * @return touch actions facade
     */
    TouchActions touch();

    /**
     * Returns categorized native-mobile actions.
     *
     * <p>The default is deliberately fail-closed. As with every Java interface default-method
     * addition, a competing unrelated zero-argument {@code mobile()} declaration with a
     * covariant-compatible return must be overridden explicitly. An existing declaration with an
     * incompatible return type is source-incompatible and cannot be bridged by an override.</p>
     *
     * @return mobile actions facade
     * @throws UnsupportedOperationException when this backend has no mobile implementation
     */
    default MobileActionsContract mobile() {
        throw new UnsupportedOperationException("Mobile actions require a live Appium session.");
    }

    /**
     * Returns alert/prompt helpers.
     *
     * @return alert actions facade
     */
    AlertActionsContract alert();

    /**
     * Returns hard-assertion builder methods scoped to this session.
     *
     * @return driver assertions facade
     */
    DriverAssertions assertThat();

    /**
     * Returns soft-verification builder methods scoped to this session.
     *
     * @return driver verifications facade
     */
    DriverVerifications verifyThat();

    /**
     * Runs a natural-language action against the current context.
     *
     * @param intent action intent
     * @param args   action arguments
     * @return this driver for fluent chaining
     */
    DriverContract act(String intent, Object... args);

    /**
     * Terminates the underlying driver session.
     */
    void quit();
}
