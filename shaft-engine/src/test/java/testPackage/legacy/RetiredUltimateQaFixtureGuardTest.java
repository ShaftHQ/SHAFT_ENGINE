package testPackage.legacy;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;

class RetiredUltimateQaFixtureGuardTest {
    private static final List<Path> NIGHTLY_FIXTURES = List.of(
            Path.of("src/test/java/testPackage/legacy/BigPageActionsTest.java"),
            Path.of("src/test/java/junitTestPackage/JunitTest.java"),
            Path.of("src/test/java/testPackage/appium/MobileWebTest.java")
    );

    @Test
    void nightlyFixturesDoNotUseTheRetiredUltimateQaForm() throws IOException {
        for (Path fixture : NIGHTLY_FIXTURES) {
            String source = Files.readString(fixture);
            assertFalse(source.contains("ultimateqa.com/complicated-page"), fixture + " still uses UltimateQA");
            assertFalse(source.contains("et_pb_contact_name_0"), fixture + " still uses the retired form");
        }
    }
}
