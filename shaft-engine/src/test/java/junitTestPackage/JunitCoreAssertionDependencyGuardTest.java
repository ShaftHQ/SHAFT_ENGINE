package junitTestPackage;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class JunitCoreAssertionDependencyGuardTest {
    private static final List<String> TESTNG_ASSERTION_IMPORTS = List.of(
            "import org.testng.Assert;",
            "import org.testng.asserts.");
    private static final List<String> ALLOWED_FILES = List.of(
            "com/shaft/validation/internal/CustomSoftAssert.java");

    @Test
    void productionCoreShouldNotUseTestNgAssertionsOutsideCompatibilityWrapper() throws IOException {
        List<String> offenders;
        try (var files = Files.walk(Path.of("src/main/java"))) {
            offenders = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".java"))
                    .filter(path -> ALLOWED_FILES.stream().noneMatch(allowed -> path.toString().replace('\\', '/').endsWith(allowed)))
                    .filter(this::containsTestNgAssertionImport)
                    .map(Path::toString)
                    .toList();
        }

        assertTrue(offenders.isEmpty(),
                "Production core must not use TestNG assertion APIs outside compatibility wrappers: " + offenders);
    }

    private boolean containsTestNgAssertionImport(Path path) {
        try {
            String content = Files.readString(path);
            return TESTNG_ASSERTION_IMPORTS.stream().anyMatch(content::contains);
        } catch (IOException e) {
            throw new AssertionError("Could not read source file: " + path, e);
        }
    }
}
