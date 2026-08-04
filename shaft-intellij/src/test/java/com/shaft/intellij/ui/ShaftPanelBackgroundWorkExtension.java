package com.shaft.intellij.ui;

import org.junit.jupiter.api.extension.DynamicTestInvocationContext;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.InvocationInterceptor;
import org.junit.jupiter.api.extension.ReflectiveInvocationContext;

import java.lang.reflect.Method;

/**
 * Ends every {@link ShaftAssistantPanel}'s background work at the end of the test that started it
 * (issue #4500).
 *
 * <p>These are plain headless JUnit tests, but the IntelliJ Platform test framework auto-registers
 * {@code ThreadLeakTrackerExtension} and {@code SwingTimerWatcherExtension} onto every test in this
 * module, and they assert per test that no thread created during it is still running work and that no
 * {@code javax.swing.Timer} is left pending. The tests here construct panels directly and drive real
 * {@code send()} flows through them, which spawn a real agent CLI child process on a bounded {@code
 * ShaftPluginExecutor} worker thread and schedule a ~100ms output-flush timer -- and then abandon the
 * panel, because a plain unit test has no Disposer tree to tear it down the way production does. The
 * abandoned run kept streaming for minutes across subsequent tests, so the two leak checks fired
 * against whichever unrelated test happened to cross the leak's boundary, a different one on every
 * run. This restores the missing per-test teardown: it calls the same production disposal
 * ({@link ShaftAssistantPanel#dispose()}) that {@code ShaftToolWindowPanel} performs at project
 * close, so the leak is ended rather than hidden -- both platform checks stay untouched and keep
 * asserting.
 *
 * <p>Implemented as an {@link InvocationInterceptor} rather than an {@code AfterEachCallback} on
 * purpose: callback extensions run in reverse registration order, and auto-detected registration
 * order across the classpath is not something this module controls, so an {@code AfterEachCallback}
 * could just as easily run after the platform's leak checks as before them. Interception of the test
 * method always completes before any {@code afterEach} callback, whatever the registration order.
 */
public final class ShaftPanelBackgroundWorkExtension implements InvocationInterceptor {
    @Override
    public void interceptTestMethod(Invocation<Void> invocation,
                                    ReflectiveInvocationContext<Method> invocationContext,
                                    ExtensionContext extensionContext) throws Throwable {
        proceedThenDisposePanels(invocation);
    }

    @Override
    public void interceptTestTemplateMethod(Invocation<Void> invocation,
                                            ReflectiveInvocationContext<Method> invocationContext,
                                            ExtensionContext extensionContext) throws Throwable {
        proceedThenDisposePanels(invocation);
    }

    /**
     * {@code @TestFactory}'s dynamic tests take their own interception hook rather than either of the
     * two above, so it is overridden here too -- the module has none today, and a first one would
     * otherwise silently lose this teardown and bring the flake back for that test kind alone.
     */
    @Override
    public void interceptDynamicTest(Invocation<Void> invocation,
                                     DynamicTestInvocationContext invocationContext,
                                     ExtensionContext extensionContext) throws Throwable {
        proceedThenDisposePanels(invocation);
    }

    private static void proceedThenDisposePanels(Invocation<Void> invocation) throws Throwable {
        try {
            invocation.proceed();
        } finally {
            ShaftAssistantPanel.disposeLivePanels();
        }
    }
}
