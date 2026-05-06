package testPackage.unitTests;

import com.shaft.listeners.TestNGListener;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;

import static org.testng.Assert.*;

public class TestNGListenerReflectionUnitTest {

    private static final Field REPORT_PORTAL_SERVICE_FIELD;

    static {
        try {
            REPORT_PORTAL_SERVICE_FIELD = TestNGListener.class.getDeclaredField("reportPortalService");
            REPORT_PORTAL_SERVICE_FIELD.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    @BeforeMethod(alwaysRun = true)
    public void resetServiceField() throws Exception {
        REPORT_PORTAL_SERVICE_FIELD.set(null, null);
    }

    @AfterMethod(alwaysRun = true)
    public void restoreServiceField() throws Exception {
        REPORT_PORTAL_SERVICE_FIELD.set(null, null);
    }

    @Test(description = "reportPortalService field is null at class-load time (no eager initialization)")
    public void testReportPortalServiceIsNotEagerlyInitialized() throws Exception {
        assertNull(REPORT_PORTAL_SERVICE_FIELD.get(null),
                "reportPortalService must be null until rp.enable=true triggers onExecutionStart");
    }

    @Test(description = "isReportPortalEnabled() returns false by default (static field initial value)")
    public void testIsReportPortalEnabledDefaultsFalse() {
        assertFalse(TestNGListener.isReportPortalEnabled(),
                "isReportPortalEnabled() must return false unless set by onExecutionStart");
    }
}
