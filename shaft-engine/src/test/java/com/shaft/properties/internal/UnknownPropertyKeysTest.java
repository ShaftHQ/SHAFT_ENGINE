package com.shaft.properties.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;

public class UnknownPropertyKeysTest {

    @Test
    public void typoOfAKnownKeyWarnsWithADidYouMean() {
        // The real downstream typo from #7085: camelCase instead of the underscore key.
        List<String> warnings = UnknownPropertyKeys.warnings("custom.properties",
                List.of("screenshotParamsWhenToTakeAScreenshot"));

        Assert.assertEquals(warnings.size(), 1, warnings.toString());
        Assert.assertTrue(warnings.getFirst().contains("did you mean \"screenshotParams_whenToTakeAScreenshot\""),
                warnings.getFirst());
    }

    @Test
    public void smallEditOfAKnownKeyWarns() {
        Assert.assertEquals(UnknownPropertyKeys.warnings("custom.properties", List.of("headlesExecution")).size(), 1);
    }

    @Test
    public void usersOwnKeysInCustomPropertiesDoNotWarn() {
        Assert.assertEquals(UnknownPropertyKeys.warnings("custom.properties",
                List.of("loginUser", "baseUrlStaging", "my.team.timeoutMinutes", "apiToken")), List.of());
    }

    @Test
    public void knownAndPassThroughKeysDoNotWarn() {
        Assert.assertEquals(UnknownPropertyKeys.warnings("custom.properties", List.of("targetBrowserName",
                "screenshotParams_whenToTakeAScreenshot", "headlessExecution", "capabilities.acceptInsecureCerts",
                "browserStack.local", "mobile_appPackage")), List.of());
    }

    @Test
    public void thirdPartyFilesAreSkipped() {
        Assert.assertEquals(UnknownPropertyKeys.warnings("log4j2.properties", List.of("headlesExecution")), List.of());
    }

    @Test
    public void bundledDefaultPropertyFilesDoNotWarn() throws IOException {
        List<String> warnings = new java.util.ArrayList<>();
        try (Stream<Path> files = Files.list(Path.of("src/main/resources/properties/default"))) {
            for (Path file : files.filter(f -> f.toString().endsWith(".properties")).toList()) {
                java.util.Properties properties = new java.util.Properties();
                try (var in = Files.newInputStream(file)) {
                    properties.load(in);
                }
                warnings.addAll(UnknownPropertyKeys.warnings(file.getFileName().toString(), properties.stringPropertyNames()));
            }
        }
        Assert.assertTrue(warnings.isEmpty(), warnings.toString());
    }

    @Test
    public void everyPropertyInterfaceIsListed() throws IOException {
        Path dir = Path.of("src/main/java/com/shaft/properties/internal");
        Set<String> declared = new TreeSet<>();
        try (Stream<Path> files = Files.list(dir)) {
            for (Path file : files.toList()) {
                if (Files.readString(file).matches("(?s).*public interface \\w+ extends EngineProperties<.*")) {
                    declared.add(file.getFileName().toString().replace(".java", ""));
                }
            }
        }
        Set<String> listed = new TreeSet<>();
        UnknownPropertyKeys.PROPERTY_INTERFACES.forEach(type -> listed.add(type.getSimpleName()));
        Assert.assertEquals(listed, declared);
    }
}
