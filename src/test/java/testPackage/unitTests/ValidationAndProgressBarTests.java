package testPackage.unitTests;

import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.ProgressBarLoggerTestAccessor;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.ValidationsExecutor;
import com.shaft.validation.internal.ValidationsExecutorTestAccessor;
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

    @Test(description = "ValidationsExecutor should clear ThreadLocal state even when performValidation fails")
    public void validationsExecutorShouldClearThreadLocalsOnFailure() throws Exception {
        ValidationsExecutor executor = new ValidationsExecutor(new com.shaft.validation.internal.ValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT));

        var driverField = ValidationsExecutor.class.getDeclaredField("driver");
        driverField.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<org.openqa.selenium.WebDriver> driver = (ThreadLocal<org.openqa.selenium.WebDriver>) driverField.get(executor);
        driver.set(null);

        var responseField = ValidationsExecutor.class.getDeclaredField("response");
        responseField.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<Object> response = (ThreadLocal<Object>) responseField.get(executor);
        response.set(new Object());
        var internalPerformMethod = ValidationsExecutor.class.getDeclaredMethod("internalPerform");
        internalPerformMethod.setAccessible(true);

        try {
            internalPerformMethod.invoke(executor);
            Assert.fail("Expected failure for unsupported validation method.");
        } catch (Exception ignored) {
            // expected
        }

        Assert.assertNull(ValidationsExecutorTestAccessor.getDriver(executor),
                "Driver ThreadLocal should be cleared even when validation fails.");
        Assert.assertNull(ValidationsExecutorTestAccessor.getResponse(executor),
                "Response ThreadLocal should be cleared even when validation fails.");
    }
}
