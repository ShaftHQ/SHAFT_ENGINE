package com.shaft.test.unittests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.ReportContext;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.IInvokedMethod;
import org.testng.ITestContext;
import org.testng.ITestListener;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.SkipException;
import org.testng.TestRunner;
import org.testng.annotations.Test;
import org.testng.internal.RuntimeBehavior;
import testsupport.ListenerListFixtures;

import java.lang.classfile.ClassFile;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

/**
 * Covers the Surefire 3.6.1 dry-run leak: a leaked {@code testng.mode.dryrun} flag is cleared
 * only for the JUnit Platform execution phase, and dry-run failures are not retried.
 *
 * <p>The real {@code org.junit.support.testng.engine.ExecutionListener} is package-private and is
 * constructed by the TestNG engine. These tests define a second class of that binary name in a
 * child loader so recognition runs without replacing the engine type.
 */
@Test(singleThreaded = true)
public class TestNGListenerDryRunUnitTest {
    private static final String DRY_RUN = RuntimeBehavior.TESTNG_MODE_DRYRUN;
    private static final String EXECUTION_LISTENER = "org.junit.support.testng.engine.ExecutionListener";
    private static final String EXECUTION_LISTENER_SUBCLASS =
            "org.junit.support.testng.engine.ExecutionListenerSubclass";
    private static final ClassLoader LISTENER_LOADER = listenerLoader();

    @Test
    public void beforeInvocationKeepsDryRunWhenTheContextIsNotATestRunner() {
        String previous = System.getProperty(DRY_RUN);
        System.setProperty(DRY_RUN, "true");
        try {
            beforeInvocation(Mockito.mock(ITestContext.class));
            assertTrue(RuntimeBehavior.isDryRun(), "Discovery without a TestRunner must stay dry.");
        } finally {
            restoreSystemProperty(DRY_RUN, previous);
            releaseInvocationState();
        }
    }

    @Test
    public void beforeInvocationKeepsDryRunWhenNoExecutionListenerIsRegistered() {
        String previous = System.getProperty(DRY_RUN);
        System.setProperty(DRY_RUN, "true");
        try {
            beforeInvocation(runnerWithNullListeners());
            assertTrue(RuntimeBehavior.isDryRun(), "A null listener list must stay dry.");

            beforeInvocation(runnerWith(List.of()));
            assertTrue(RuntimeBehavior.isDryRun(), "An empty listener list must stay dry.");

            beforeInvocation(runnerWith(ListenerListFixtures.listWithNullElement(
                    new UnrelatedListener(), new UnrelatedListenerSubclass())));
            assertTrue(RuntimeBehavior.isDryRun(), "Unrelated listeners, including null, must stay dry.");
        } finally {
            restoreSystemProperty(DRY_RUN, previous);
            releaseInvocationState();
        }
    }

    @Test
    public void beforeInvocationKeepsDryRunWhenTheFlagIsOff() {
        String previous = System.getProperty(DRY_RUN);
        System.clearProperty(DRY_RUN);
        try {
            beforeInvocation(runnerWith(List.of(newExecutionListener(EXECUTION_LISTENER))));
            assertNull(System.getProperty(DRY_RUN), "A real run must not invent a dry-run flag.");
        } finally {
            restoreSystemProperty(DRY_RUN, previous);
            releaseInvocationState();
        }
    }

    @Test
    public void beforeInvocationClearsDryRunForTheExecutionListenerAndItsSubclass() {
        String previous = System.getProperty(DRY_RUN);
        try {
            System.setProperty(DRY_RUN, "true");
            beforeInvocation(runnerWith(List.of(new UnrelatedListener(), newExecutionListener(EXECUTION_LISTENER))));
            assertFalse(RuntimeBehavior.isDryRun(), "The execution listener must clear a leaked dry-run flag.");

            System.setProperty(DRY_RUN, "true");
            beforeInvocation(runnerWith(List.of(newExecutionListener(EXECUTION_LISTENER_SUBCLASS))));
            assertFalse(RuntimeBehavior.isDryRun(), "A subclass of the execution listener must clear the flag.");
        } finally {
            restoreSystemProperty(DRY_RUN, previous);
            releaseInvocationState();
        }
    }

    @Test
    public void retryReturnsFalseWhenTestNgDryRunIsEnabled() {
        String previous = System.getProperty(DRY_RUN);
        int originalRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        ITestNGMethod method = Mockito.mock(ITestNGMethod.class);
        Mockito.when(method.getMethodName()).thenReturn("expectedException");
        ITestResult result = Mockito.mock(ITestResult.class);
        Mockito.when(result.getMethod()).thenReturn(method);
        Mockito.when(result.getThrowable()).thenReturn(new AssertionError("missing expected exception"));
        try {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);
            System.setProperty(DRY_RUN, "true");
            assertFalse(new RetryAnalyzer().retry(result), "A dry-run failure is not a real attempt.");
            System.clearProperty(DRY_RUN);
            assertTrue(new RetryAnalyzer().retry(result), "The same failure retries once the dry-run flag is gone.");
        } finally {
            restoreSystemProperty(DRY_RUN, previous);
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetries);
            Properties.clearForCurrentThread();
        }
    }

    private static TestRunner runnerWith(List<ITestListener> listeners) {
        TestRunner runner = Mockito.mock(TestRunner.class);
        Mockito.when(runner.getTestListeners()).thenReturn(listeners);
        return runner;
    }

    private static TestRunner runnerWithNullListeners() {
        TestRunner runner = Mockito.mock(TestRunner.class);
        Mockito.when(runner.getTestListeners()).thenReturn(null);
        return runner;
    }

    private static ITestListener newExecutionListener(String binaryName) {
        try {
            Class<?> type = LISTENER_LOADER.loadClass(binaryName);
            return (ITestListener) type.getDeclaredConstructor().newInstance();
        } catch (ReflectiveOperationException failure) {
            throw new IllegalStateException(failure);
        }
    }

    /**
     * Defines the execution-listener name in a child loader. The parent loader still resolves the
     * package-private engine class that Surefire instantiates.
     */
    private static ClassLoader listenerLoader() {
        ClassDesc listener = ClassDesc.of(EXECUTION_LISTENER);
        ClassDesc subclass = ClassDesc.of(EXECUTION_LISTENER_SUBCLASS);
        MethodTypeDesc init = MethodTypeDesc.of(ConstantDescs.CD_void);
        byte[] listenerBytes = ClassFile.of().build(listener, builder -> {
            builder.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_SUPER);
            builder.withInterfaceSymbols(ClassDesc.of("org.testng.ITestListener"));
            builder.withMethodBody("<init>", init, ClassFile.ACC_PUBLIC, code -> {
                code.aload(0);
                code.invokespecial(ConstantDescs.CD_Object, "<init>", init);
                code.return_();
            });
        });
        byte[] subclassBytes = ClassFile.of().build(subclass, builder -> {
            builder.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_SUPER);
            builder.withSuperclass(listener);
            builder.withMethodBody("<init>", init, ClassFile.ACC_PUBLIC, code -> {
                code.aload(0);
                code.invokespecial(listener, "<init>", init);
                code.return_();
            });
        });
        Map<String, byte[]> classes = Map.of(EXECUTION_LISTENER, listenerBytes, EXECUTION_LISTENER_SUBCLASS, subclassBytes);
        return new IsolatedListenerLoader(classes);
    }

    private static void beforeInvocation(ITestContext context) {
        TestNGListener listener = new TestNGListener();
        IInvokedMethod invoked = Mockito.mock(IInvokedMethod.class);
        ITestNGMethod method = Mockito.mock(ITestNGMethod.class);
        ITestResult result = Mockito.mock(ITestResult.class);
        Mockito.when(invoked.getTestMethod()).thenReturn(method);
        Mockito.when(result.getMethod()).thenReturn(method);
        Mockito.when(method.getMethodName()).thenReturn("dryRunProbe");
        try (MockedStatic<TestNGListenerHelper> ignoredHelper = Mockito.mockStatic(TestNGListenerHelper.class);
             MockedStatic<JiraHelper> ignoredJira = Mockito.mockStatic(JiraHelper.class);
             MockedStatic<RetryAnalyzer> ignoredRetry = Mockito.mockStatic(RetryAnalyzer.class)) {
            try {
                listener.beforeInvocation(invoked, result, context);
            } catch (SkipException timeout) {
                // The suite clock is unset here, so the timeout fires after the dry-run release.
                String message = timeout.getMessage();
                assertTrue(message != null && message.contains("timeout"));
            }
        }
    }

    private static void releaseInvocationState() {
        ReportContext.clear();
        ListenerListFixtures.clearBoundTestResult();
        Properties.clearForCurrentThread();
    }

    private static void restoreSystemProperty(String key, String value) {
        if (value == null) {
            System.clearProperty(key);
        } else {
            System.setProperty(key, value);
        }
    }

    public static class UnrelatedListener implements ITestListener {
    }

    public static class UnrelatedListenerSubclass extends UnrelatedListener {
    }

    private static final class IsolatedListenerLoader extends ClassLoader {
        private final Map<String, byte[]> classes;

        private IsolatedListenerLoader(Map<String, byte[]> classes) {
            super(TestNGListenerDryRunUnitTest.class.getClassLoader());
            this.classes = classes;
        }

        @Override
        protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
            byte[] bytes = classes.get(name);
            if (bytes == null) {
                return super.loadClass(name, resolve);
            }
            synchronized (getClassLoadingLock(name)) {
                Class<?> loaded = findLoadedClass(name);
                if (loaded == null) {
                    loaded = defineClass(name, bytes, 0, bytes.length);
                }
                if (resolve) {
                    resolveClass(loaded);
                }
                return loaded;
            }
        }
    }
}
