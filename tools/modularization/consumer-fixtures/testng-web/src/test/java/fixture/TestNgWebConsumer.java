package fixture;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.TestNGListener;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.ITestNGListener;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ServiceLoader;

public class TestNgWebConsumer {
    @Test
    public void shaftAssertionPassesInTestNgConsumerProject() {
        SHAFT.Validations.assertThat().object("SHAFT").isEqualTo("SHAFT").perform();
    }

    @Test
    public void expectedShaftAssertionFailureIsAssertionErrorInTestNgConsumerProject() {
        Assert.expectThrows(AssertionError.class,
                () -> SHAFT.Validations.assertThat().object("actual").isEqualTo("expected").perform());
    }

    @Test
    public void softVerificationFailureIsAvailableToTestNgListenerPath() {
        SHAFT.Validations.verifyThat().object("actual").isEqualTo("expected").perform();

        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    public void testNgListenerIsVisibleThroughSpi() {
        boolean listenerAvailable = ServiceLoader.load(ITestNGListener.class).stream()
                .anyMatch(provider -> provider.type().equals(TestNGListener.class));

        Assert.assertTrue(listenerAvailable);
    }

    @AfterSuite(alwaysRun = true)
    public void generatedAllureEvidenceExists() {
        Assert.assertTrue(Files.isDirectory(Path.of("allure-results")));
    }
}
