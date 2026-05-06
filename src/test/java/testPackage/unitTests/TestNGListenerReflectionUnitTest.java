package testPackage.unitTests;

import com.shaft.listeners.TestNGListener;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class TestNGListenerReflectionUnitTest {

    @Test(description = "TestNGListener loads without ReportPortal service being eagerly initialized")
    public void testReportPortalServiceIsNotEagerlyInitialized() throws Exception {
        // Verify the field exists and is accessible by name (reflective contract)
        var field = TestNGListener.class.getDeclaredField("reportPortalService");
        assertNotNull(field);
        field.setAccessible(true);
        // When rp.enable is not set, the service must be null — no eager initialization
        assertNull(field.get(null), "reportPortalService must be null until rp.enable=true is set");
    }

    @Test(description = "isReportPortalEnabled defaults to false without rp.enable property")
    public void testIsReportPortalEnabledDefaultsFalse() {
        System.clearProperty("rp.enable");
        assertFalse(TestNGListener.isReportPortalEnabled(),
                "isReportPortalEnabled should be false when rp.enable is not set");
    }
}
