package testPackage.unitTests;

import com.shaft.driver.internal.BrowserSessionReuse;
import com.shaft.listeners.TestNGListener;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.ITestClass;
import org.testng.annotations.Test;

/**
 * Unit coverage for the class-end hook that quits browsers kept by {@code reuseBrowserSession}.
 */
public class BrowserSessionReuseListenerTest {
    /**
     * The hook is safe when no browser is cached and reuse stays off by default.
     */
    @Test
    public void onAfterClassShouldBeNoOpWithoutReusedSessions() {
        ITestClass testClass = Mockito.mock(ITestClass.class);
        Mockito.when(testClass.getName()).thenReturn("some.Class");
        new TestNGListener().onAfterClass(testClass);
        Assert.assertNull(BrowserSessionReuse.reusable());
        Assert.assertFalse(BrowserSessionReuse.resetInsteadOfQuit(null));
    }
}
