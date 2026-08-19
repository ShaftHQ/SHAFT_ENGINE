package testPackage.locator;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * Inventory guard for #5207: Local E2E GUI tests must not depend on the
 * public practice-test-login host that failed Safari nightly (#5204).
 */
public class LocalE2EPublicLoginHostGuardTest {
    private static final String FORBIDDEN_HOST = "practicetestautomation" + ".com";

    @Test
    public void localE2EGuiTestsMustNotUsePracticeTestAutomation() throws IOException {
        Path testRoot = Path.of("src/test/java");
        Assert.assertTrue(Files.isDirectory(testRoot),
                "expected shaft-engine test sources at " + testRoot.toAbsolutePath());

        List<String> hits = new ArrayList<>();
        try (Stream<Path> files = Files.walk(testRoot)) {
            files.filter(path -> path.toString().endsWith(".java"))
                    .filter(LocalE2EPublicLoginHostGuardTest::isLocalE2EGuiTest)
                    .forEach(path -> addHitIfForbidden(testRoot, path, hits));
        }

        Assert.assertTrue(hits.isEmpty(),
                "Local E2E GUI tests still use " + FORBIDDEN_HOST + ": " + hits);
    }

    private static boolean isLocalE2EGuiTest(Path path) {
        String normalized = path.toString().replace('\\', '/');
        return !normalized.contains("/unitTests/")
                && !normalized.contains("/LambdaTest/")
                && !normalized.contains("/appium/");
    }

    private static void addHitIfForbidden(Path testRoot, Path path, List<String> hits) {
        try {
            if (Files.readString(path).contains(FORBIDDEN_HOST)) {
                hits.add(testRoot.relativize(path).toString().replace('\\', '/'));
            }
        } catch (IOException exception) {
            Assert.fail(exception.getMessage(), exception);
        }
    }
}
