package testPackage;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class ThreadLocalGuiTeardownGuardTest {

    @Test(description = "ThreadLocal GUI tests must null-guard quit() and call driver.remove()")
    public void threadLocalGuiTeardownsMustQuitThenRemove() throws IOException {
        Path testRoot = Path.of("src/test/java");
        if (!Files.isDirectory(testRoot)) {
            testRoot = Path.of("shaft-engine/src/test/java");
        }
        Assert.assertTrue(Files.isDirectory(testRoot), testRoot.toAbsolutePath().toString());

        List<String> missingRemove = new ArrayList<>();
        List<String> missingNullGuard = new ArrayList<>();
        try (Stream<Path> paths = Files.walk(testRoot)) {
            for (Path path : paths.filter(candidate -> candidate.toString().endsWith(".java")).toList()) {
                String text = Files.readString(path);
                if (!text.contains("ThreadLocal<") || !text.contains("driver.get().quit()")) {
                    continue;
                }
                String relative = testRoot.relativize(path).toString().replace('\\', '/');
                if (!text.contains("driver.remove()")) {
                    missingRemove.add(relative);
                }
                if (!text.contains("driver.get() != null")) {
                    missingNullGuard.add(relative);
                }
            }
        }
        Assert.assertTrue(missingRemove.isEmpty(), "ThreadLocal GUI tests missing driver.remove(): " + missingRemove);
        Assert.assertTrue(missingNullGuard.isEmpty(),
                "ThreadLocal GUI tests missing null-guard before quit(): " + missingNullGuard);
    }
}
