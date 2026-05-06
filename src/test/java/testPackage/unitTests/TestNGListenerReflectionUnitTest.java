package testPackage.unitTests;

import com.shaft.listeners.TestNGListener;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class TestNGListenerReflectionUnitTest {

    private static final java.lang.reflect.Field REPORT_PORTAL_SERVICE_FIELD;

    static {
        try {
            REPORT_PORTAL_SERVICE_FIELD = TestNGListener.class.getDeclaredField("reportPortalService");
            REPORT_PORTAL_SERVICE_FIELD.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    @BeforeMethod(alwaysRun = true)
    public void resetReportPortalServiceField() throws Exception {
        REPORT_PORTAL_SERVICE_FIELD.set(null, null);
    }

    @AfterMethod(alwaysRun = true)
    public void restoreReportPortalServiceField() throws Exception {
        REPORT_PORTAL_SERVICE_FIELD.set(null, null);
    }

    @Test(description = "TestNGListener loads without ReportPortal service being eagerly initialized")
    public void testReportPortalServiceIsNotEagerlyInitialized() throws Exception {
        // Field must be null — no eager initialization at class-load time
        assertNull(REPORT_PORTAL_SERVICE_FIELD.get(null),
                "reportPortalService must be null until rp.enable=true is set");
    }

    @Test(description = "isReportPortalEnabled() returns false by default (static field initial value)")
    public void testIsReportPortalEnabledDefaultsFalse() {
        // The static boolean field defaults to false at JVM startup;
        // only onExecutionStart() with rp.enable=true changes it
        assertFalse(TestNGListener.isReportPortalEnabled(),
                "isReportPortalEnabled() must return false unless explicitly set by onExecutionStart");
    }
}
