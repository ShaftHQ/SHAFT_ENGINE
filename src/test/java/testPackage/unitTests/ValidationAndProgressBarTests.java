package testPackage.unitTests;

import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.ProgressBarLoggerTestAccessor;
import com.shaft.validation.internal.ValidationsHelperTestAccessor;
import com.shaft.validation.internal.ValidationsHelper;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

public class ValidationAndProgressBarTests {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "resetVerificationStateAfterFailing should clear helper ThreadLocal state")
    public void resetVerificationStateShouldClearThreadLocalState() throws Exception {
        ValidationsHelperTestAccessor.setOptionalCustomLogMessage(new ArrayList<>(List.of("custom log")));
        ValidationsHelperTestAccessor.setLastUsedElementLocator(By.id("sample"));

        ValidationsHelper.resetVerificationStateAfterFailing();

        Assert.assertNull(ValidationsHelperTestAccessor.getOptionalCustomLogMessage(),
                "Custom log message ThreadLocal should be cleared to avoid retaining data across pooled threads.");
        Assert.assertNull(ValidationsHelperTestAccessor.getLastUsedElementLocator(),
                "Last-used locator ThreadLocal should be cleared to avoid retaining element references.");
    }

    @Test(description = "ProgressBarLogger should disable ANSI colors when cucumber.ansi-colors.disabled is true")
    public void shouldUseAnsiColorsShouldReturnFalseWhenAnsiIsDisabled() {
        ThreadLocalPropertiesManager.setProperty("cucumber.ansi-colors.disabled", "true");
        boolean actual = ProgressBarLoggerTestAccessor.isAnsiColorEnabledForCurrentEnvironment();
        Assert.assertFalse(actual, "ANSI colors should be disabled when cucumber.ansi-colors.disabled=true.");
    }
}
