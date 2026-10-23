package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.AllureManager;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;

/**
 * #7320: rebuilding a report from a standalone {@code main} (no listener ran) must load SHAFT
 * properties first instead of failing with a {@link NullPointerException}. The global base configs are
 * not nulled here because other test classes may read them concurrently; the uninitialized flag is.
 */
@Test(singleThreaded = true)
public class AllureReportPropertiesUnloadedUnitTest {
    @AfterMethod(alwaysRun = true)
    public void restore() throws ReflectiveOperationException {
        if (!Properties.isInitialized()) {
            initializedFlag().setBoolean(null, true);
        }
    }

    @Test
    public void reportRebuildLoadsPropertiesWhenNoListenerDid() throws ReflectiveOperationException {
        initializedFlag().setBoolean(null, false);

        AllureManager.ensurePropertiesLoaded();

        Assert.assertTrue(Properties.isInitialized(), "generateReport must initialize SHAFT properties first");
        Assert.assertNotNull(SHAFT.Properties.paths.allureResults());
    }

    @Test
    public void loadedPropertiesAreNotReinitialized() {
        Assert.assertTrue(Properties.isInitialized() || initializeForTest());
        String before = SHAFT.Properties.paths.allureResults();
        AllureManager.ensurePropertiesLoaded();
        Assert.assertEquals(SHAFT.Properties.paths.allureResults(), before);
    }

    private static boolean initializeForTest() {
        AllureManager.ensurePropertiesLoaded();
        return Properties.isInitialized();
    }

    private static Field initializedFlag() throws NoSuchFieldException {
        Field field = Properties.class.getDeclaredField("initialized");
        field.setAccessible(true);
        return field;
    }
}
