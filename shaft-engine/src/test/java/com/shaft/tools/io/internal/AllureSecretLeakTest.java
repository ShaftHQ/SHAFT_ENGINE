package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureExternalKey;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.FileSystemResultsWriter;
import io.qameta.allure.model.Parameter;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResult;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

/**
 * Proves that SHAFT itself never writes secret values into allure-results (#7173): typed text that
 * is a registered secret or a JWT is masked in step names, parameters and logs, files named
 * {@code *secret*.properties} are never attached, and their values stay out of {@code environment.xml}.
 */
@Test(singleThreaded = true)
public class AllureSecretLeakTest {
    private static final String SECRET_VALUE = "REPRO_SECRET_VALUE_123456";
    private static final String PASSWORD_VALUE = "CustomPassw0rdValue";
    private static final String JWT = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJyZXBybyJ9.c2lnbmF0dXJlLXJlcHJv";
    private static final String MASK = "********";

    private Path workDirectory;
    private Path resultsDirectory;
    private String originalPropertiesPath;
    private String originalResultsPath;
    private AllureLifecycle originalLifecycle;

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws IOException {
        workDirectory = Files.createTempDirectory("shaft-secret-leak");
        Path propertiesDirectory = Files.createDirectories(workDirectory.resolve("properties"));
        resultsDirectory = Files.createDirectories(workDirectory.resolve("allure-results"));
        Files.writeString(propertiesDirectory.resolve("secrets.properties"),
                "api.clientSecret=" + SECRET_VALUE + System.lineSeparator(), StandardCharsets.UTF_8);
        Files.writeString(propertiesDirectory.resolve("custom.properties"),
                "screenshotParams_whenToTakeAScreenshot=Always" + System.lineSeparator()
                        + "repro.user.password=" + PASSWORD_VALUE + System.lineSeparator()
                        + "repro.reportNote=visible-note" + System.lineSeparator(), StandardCharsets.UTF_8);
        originalPropertiesPath = SHAFT.Properties.paths.properties();
        originalResultsPath = SHAFT.Properties.paths.allureResults();
        SHAFT.Properties.paths.set().properties(propertiesDirectory + File.separator);
        SHAFT.Properties.paths.set().allureResults(resultsDirectory + File.separator);
        originalLifecycle = Allure.getLifecycle();
        Allure.setLifecycle(new AllureLifecycle(new FileSystemResultsWriter(resultsDirectory)));
        PropertyFileManager.readCustomPropertyFiles();
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws IOException {
        Allure.setLifecycle(originalLifecycle);
        System.clearProperty("api.clientSecret");
        System.clearProperty("repro.user.password");
        System.clearProperty("repro.reportNote");
        System.clearProperty("screenshotParams_whenToTakeAScreenshot");
        ReportSecrets.clear();
        SHAFT.Properties.paths.set().properties(originalPropertiesPath);
        SHAFT.Properties.paths.set().allureResults(originalResultsPath);
        Properties.clearForCurrentThread();
        try (Stream<Path> paths = Files.walk(workDirectory)) {
            paths.sorted(Comparator.reverseOrder()).map(Path::toFile).forEach(File::delete);
        }
    }

    @Test
    public void engineSetupNeverAttachesSecretsFileOrItsValues() throws IOException {
        runInsideTestCase(ReportHelper::attachPropertyFiles);

        String results = readAllResults();
        assertNoSecrets(results);
        Assert.assertFalse(results.contains("\"secrets\""), "secrets.properties must never be attached");
        Assert.assertTrue(results.contains("visible-note"), "non-secret property files must still be attached");
    }

    @Test
    public void environmentXmlCarriesNoSecretValues() throws IOException {
        AllureManager.writeEnvironmentVariablesToAllureResultsDirectory();

        String environment = Files.readString(resultsDirectory.resolve("environment.xml"), StandardCharsets.UTF_8);
        assertNoSecrets(environment);
        Assert.assertFalse(environment.contains("api.clientSecret"), "keys from secrets.properties are omitted");
        Assert.assertTrue(environment.contains("repro.reportNote"), "ordinary properties stay in environment.xml");
    }

    @Test
    public void typedSecretsAreMaskedInStepNamesParametersAndLogs() throws IOException {
        runInsideTestCase(() -> {
            ReportManager.log("Type \"" + JWT + "\" into \"q\".");
            ReportManager.log("Type \"" + SECRET_VALUE + "\" into \"client secret\".");
            var lifecycle = Allure.getLifecycle();
            AllureExternalKey stepKey = AllureExternalKey.of(AllureSecretLeakTest.class, UUID.randomUUID().toString());
            lifecycle.startStep(stepKey, new StepResult().setName("Type \"" + JWT + "\" into \"q\"")
                    .setParameters(new ArrayList<>(List.of(new Parameter().setName("text").setValue(JWT)))));
            lifecycle.stopStep(stepKey);
            ReportManagerHelper.attachTestLog("typeToken", String.join("\n", ReportContext.snapshotOutput()));
        });

        String results = readAllResults();
        assertNoSecrets(results);
        Assert.assertTrue(results.contains("Type \\\"" + MASK + "\\\" into"), "typed secrets render as the mask");
    }

    @Test
    public void ordinaryTypedTextIsNotMasked() throws IOException {
        runInsideTestCase(() -> ReportManager.log("Type \"duckduckgo search\" into \"q\"."));
        Assert.assertTrue(readAllResults().contains("duckduckgo search"));
    }

    private void runInsideTestCase(Runnable body) {
        var lifecycle = Allure.getLifecycle();
        String uuid = UUID.randomUUID().toString();
        AllureExternalKey testKey = AllureExternalKey.of(AllureSecretLeakTest.class, uuid);
        lifecycle.scheduleTest(testKey, new TestResult().setUuid(uuid).setName("secretLeak"));
        lifecycle.startTest(testKey);
        try {
            ReportContext.clear();
            body.run();
        } finally {
            lifecycle.stopTest(testKey);
            lifecycle.writeTest(testKey);
        }
    }

    private String readAllResults() throws IOException {
        StringBuilder all = new StringBuilder();
        try (Stream<Path> files = Files.walk(resultsDirectory)) {
            for (Path file : files.filter(Files::isRegularFile).toList()) {
                all.append(Files.readString(file, StandardCharsets.ISO_8859_1)).append('\n');
            }
        }
        return all.toString();
    }

    private static void assertNoSecrets(String text) {
        Assert.assertFalse(text.contains(SECRET_VALUE), "secrets.properties value leaked");
        Assert.assertFalse(text.contains(PASSWORD_VALUE), "password-named property value leaked");
        Assert.assertFalse(text.contains("eyJhbGciOiJIUzI1NiJ9"), "JWT leaked");
    }
}
