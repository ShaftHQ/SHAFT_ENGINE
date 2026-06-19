package fixture;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.JunitExtension;
import com.shaft.listeners.JunitListener;
import com.shaft.validation.internal.ValidationsHelper;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.Extension;
import org.junit.platform.launcher.LauncherSessionListener;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ServiceLoader;

class JUnitWebConsumer {
    @Test
    void shaftAssertionPassesInJunitConsumerProject() {
        SHAFT.Validations.assertThat().object("SHAFT").isEqualTo("SHAFT").perform();
    }

    @Test
    void expectedShaftAssertionFailureIsAssertionErrorInJunitConsumerProject() {
        Assertions.assertThrows(AssertionError.class,
                () -> SHAFT.Validations.assertThat().object("actual").isEqualTo("expected").perform());
    }

    @Test
    void softVerificationFailureIsAvailableToJunitExtensionPath() {
        SHAFT.Validations.verifyThat().object("actual").isEqualTo("expected").perform();

        Assertions.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test
    void junitListenerAndExtensionAreVisibleThroughSpi() {
        boolean listenerAvailable = ServiceLoader.load(LauncherSessionListener.class).stream()
                .anyMatch(provider -> provider.type().equals(JunitListener.class));
        boolean extensionAvailable = ServiceLoader.load(Extension.class).stream()
                .anyMatch(provider -> provider.type().equals(JunitExtension.class));

        Assertions.assertTrue(listenerAvailable);
        Assertions.assertTrue(extensionAvailable);
    }

    @AfterAll
    static void generatedAllureEvidenceExists() {
        Assertions.assertTrue(Files.isDirectory(Path.of("allure-results")));
    }
}
