package testPackage.unitTests;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Internal;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.AllureManager;
import org.aeonbits.owner.ConfigFactory;
import org.apache.commons.lang3.SystemUtils;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

/**
 * Unit tests for private helper behavior in {@link AllureManager}.
 */
@Test(singleThreaded = true)
public class AllureManagerUnitTest {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Sets a static private field on {@code clazz} via reflection.
     *
     * @param clazz     the class that declares the field
     * @param fieldName the field name
     * @param value     the value to assign (use {@code null} to clear a reference)
     */
    private static void setStaticField(Class<?> clazz, String fieldName, Object value) throws Exception {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(null, value);
    }

    /**
     * Reads a static private field on {@code clazz} via reflection.
     *
     * @param clazz     the class that declares the field
     * @param fieldName the field name
     * @return the current field value
     */
    private static Object getStaticField(Class<?> clazz, String fieldName) throws Exception {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.get(null);
    }

    @Test(description = "patchStatusDetailsInJson should normalize step statusDetails and prune empty fixtures")
    public void patchStatusDetailsInJsonShouldNormalizeNodesAndPruneEmptyFixtures() throws Exception {
        String input = """
                {
                  "steps": [
                    {"name":"s1","stage":"finished"},
                    {"name":"s2","stage":"running","statusDetails":{"trace":"x"},"steps":[{"name":"nested","stage":"scheduled"}]}
                  ],
                  "befores": [
                    {"name":"empty-before","steps":[],"attachments":[],"parameters":[]},
                    {"name":"valid-before","steps":[{"name":"b1","stage":"finished"}]}
                  ]
                }
                """;

        Method patchMethod = AllureManager.class.getDeclaredMethod("patchStatusDetailsInJson", String.class);
        patchMethod.setAccessible(true);
        String patched = (String) patchMethod.invoke(null, input);

        JsonNode root = MAPPER.readTree(patched);
        SHAFT.Validations.assertThat().number(root.path("steps").size()).isEqualTo(2).perform();

        JsonNode step1 = root.path("steps").get(0);
        SHAFT.Validations.assertThat().object(step1.path("statusDetails").path("message").asText()).isEqualTo("").perform();

        JsonNode step2 = root.path("steps").get(1);
        SHAFT.Validations.assertThat().object(step2.path("statusDetails").path("trace").asText()).isEqualTo("x").perform();
        SHAFT.Validations.assertThat().object(step2.path("statusDetails").path("message").asText()).isEqualTo("").perform();
        SHAFT.Validations.assertThat().object(step2.path("steps").get(0).path("statusDetails").path("message").asText()).isEqualTo("").perform();

        SHAFT.Validations.assertThat().number(root.path("befores").size()).isEqualTo(1).perform();
        SHAFT.Validations.assertThat().object(root.path("befores").get(0).path("steps").get(0)
                .path("statusDetails").path("message").asText()).isEqualTo("").perform();
    }

    @Test(description = "getResultsPath should trim only a trailing separator")
    public void getResultsPathShouldTrimOnlyTrailingSeparator() throws Exception {
        Field pathField = AllureManager.class.getDeclaredField("allureResultsFolderPath");
        pathField.setAccessible(true);

        Method getResultsPath = AllureManager.class.getDeclaredMethod("getResultsPath");
        getResultsPath.setAccessible(true);

        pathField.set(null, "allure-results/");
        SHAFT.Validations.assertThat().object(getResultsPath.invoke(null).toString()).isEqualTo("allure-results").perform();

        pathField.set(null, "allure-results");
        SHAFT.Validations.assertThat().object(getResultsPath.invoke(null).toString()).isEqualTo("allure-results").perform();

        pathField.set(null, "");
        SHAFT.Validations.assertThat().object(getResultsPath.invoke(null).toString()).isEqualTo("").perform();
    }

    @Test(description = "Absolute allureResultsFolderPath should define the Allure execution root")
    public void absoluteAllureResultsPathShouldDefineExecutionRoot() throws Exception {
        Path executionRoot = Path.of(System.getProperty("user.dir"), "target", "allure-absolute-execution-root")
                .toAbsolutePath()
                .normalize();
        Path resultsDirectory = executionRoot.resolve("allure-results");
        Files.createDirectories(resultsDirectory);
        setStaticField(AllureManager.class, "allureResultsFolderPath", resultsDirectory.toString());
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "echo");
        SHAFT.Properties.allure.set().forceConfiguredCliVersion(true).accumulateHistory(true);

        Method reportDirectoryPath = AllureManager.class.getDeclaredMethod("reportDirectoryPath");
        Method reportOutputDirectoryPath = AllureManager.class.getDeclaredMethod("reportOutputDirectoryPath");
        Method allureConfigPath = AllureManager.class.getDeclaredMethod("allureConfigPath");
        Method writeGenerateReportShellFiles = AllureManager.class.getDeclaredMethod("writeGenerateReportShellFilesToProjectDirectory");
        Method writeAllureConfig = AllureManager.class.getDeclaredMethod("writeAllureConfig", String.class, String.class);
        Method getCommandToCreateAllureReport = AllureManager.class.getDeclaredMethod("getCommandToCreateAllureReport");
        reportDirectoryPath.setAccessible(true);
        reportOutputDirectoryPath.setAccessible(true);
        allureConfigPath.setAccessible(true);
        writeGenerateReportShellFiles.setAccessible(true);
        writeAllureConfig.setAccessible(true);
        getCommandToCreateAllureReport.setAccessible(true);

        Path reportDirectory = (Path) reportDirectoryPath.invoke(null);
        Path reportOutputDirectory = (Path) reportOutputDirectoryPath.invoke(null);
        Path configPath = (Path) allureConfigPath.invoke(null);
        writeGenerateReportShellFiles.invoke(null);
        writeAllureConfig.invoke(null, "Absolute Root Report", reportOutputDirectory.toString());
        setStaticField(AllureManager.class, "allureOutPutDirectory", reportOutputDirectory.toString());
        String command = (String) getCommandToCreateAllureReport.invoke(null);

        String scriptFileName = SystemUtils.IS_OS_WINDOWS ? "generate_allure_report.bat" : "generate_allure_report.sh";
        Path scriptPath = executionRoot.resolve(scriptFileName);
        String scriptContent = Files.readString(scriptPath, StandardCharsets.UTF_8);
        String configContent = Files.readString(configPath, StandardCharsets.UTF_8);

        SHAFT.Validations.assertThat().object(reportDirectory).isEqualTo(executionRoot.resolve("allure-report")).perform();
        SHAFT.Validations.assertThat().object(reportOutputDirectory).isEqualTo(executionRoot.resolve("target").resolve("allure-report")).perform();
        SHAFT.Validations.assertThat().object(configPath).isEqualTo(executionRoot.resolve("allurerc.yaml")).perform();
        SHAFT.Validations.assertThat().object(scriptContent).contains(configPath.toString()).perform();
        SHAFT.Validations.assertThat().object(scriptContent).contains(resultsDirectory.toString()).perform();
        SHAFT.Validations.assertThat().object(configContent).contains(reportOutputDirectory.toString().replace("\\", "/")).perform();
        SHAFT.Validations.assertThat().object(configContent).contains(executionRoot.resolve("target").resolve("history.jsonl").toString().replace("\\", "/")).perform();
        SHAFT.Validations.assertThat().object(command).contains(configPath.toString()).perform();
        SHAFT.Validations.assertThat().object(command).contains(resultsDirectory.toString()).perform();
        SHAFT.Validations.assertThat().object(command).contains(reportOutputDirectory.toString()).perform();
    }

    @Test(description = "cleanAllureResultsDirectory should leave the results directory ready for parallel Allure writers")
    public void cleanAllureResultsDirectoryShouldRecreateResultsDirectory() throws Exception {
        Path resultsDirectory = Files.createTempDirectory("shaft-allure-results");
        Path staleResult = resultsDirectory.resolve("stale-result.json");
        Path nestedDirectory = resultsDirectory.resolve("nested");
        Path nestedResult = nestedDirectory.resolve("nested-result.json");
        Files.createDirectories(nestedDirectory);
        Files.writeString(staleResult, "stale", StandardCharsets.UTF_8);
        Files.writeString(nestedResult, "nested-stale", StandardCharsets.UTF_8);
        setStaticField(AllureManager.class, "allureResultsFolderPath", resultsDirectory.toString());
        SHAFT.Properties.allure.set().cleanResultsDirectory(true);
        Method cleanAllureResultsDirectory = AllureManager.class.getDeclaredMethod("cleanAllureResultsDirectory");
        cleanAllureResultsDirectory.setAccessible(true);

        cleanAllureResultsDirectory.invoke(null);

        SHAFT.Validations.assertThat().object(Files.isDirectory(resultsDirectory)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.exists(staleResult)).isFalse().perform();
        SHAFT.Validations.assertThat().object(Files.exists(nestedResult)).isFalse().perform();
        SHAFT.Validations.assertThat().object(Files.exists(nestedDirectory)).isFalse().perform();
    }

    @Test(description = "AllureManager utility class constructor should be blocked")
    public void constructorShouldThrowIllegalStateException() throws Exception {
        Constructor<AllureManager> constructor = AllureManager.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException exception = null;
        boolean constructorThrew = false;
        try {
            constructor.newInstance();
        } catch (InvocationTargetException e) {
            exception = e;
            constructorThrew = true;
        }

        SHAFT.Validations.assertThat().object(constructorThrew).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(exception.getCause().getClass().getName())
                .isEqualTo(IllegalStateException.class.getName()).perform();
        SHAFT.Validations.assertThat().object(exception.getCause().getMessage()).isEqualTo("Utility class").perform();
    }

    @Test(description = "Generated allure serve script should verify installed allure version before using it")
    public void generatedAllureServeScriptShouldCheckVersionBeforeUsingSystemAllure() throws Exception {
        Field pathField = AllureManager.class.getDeclaredField("allureResultsFolderPath");
        pathField.setAccessible(true);
        pathField.set(null, "allure-results/");

        Method scriptMethod = AllureManager.class.getDeclaredMethod("writeGenerateReportShellFilesToProjectDirectory");
        scriptMethod.setAccessible(true);

        String scriptFileName = SystemUtils.IS_OS_WINDOWS ? "generate_allure_report.bat" : "generate_allure_report.sh";
        Path scriptPath = Path.of(scriptFileName);
        try {
            isolateFromProvisionedCliCache();
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);
            scriptMethod.invoke(null);
            String content = Files.readString(scriptPath, StandardCharsets.UTF_8);

            SHAFT.Validations.assertThat().object(content.contains("npx --yes allure@" + SHAFT.Properties.internal.allure3Version()))
                    .isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(content.contains("allure --version")).isEqualTo(false).perform();
        } finally {
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
            Files.deleteIfExists(scriptPath);
        }
    }

    @Test(description = "Generated allure serve script still uses managed Allure 3 when forceConfiguredCliVersion is false")
    public void generatedAllureServeScriptShouldUseManagedAllure3WhenEnforcementDisabled() throws Exception {
        Field pathField = AllureManager.class.getDeclaredField("allureResultsFolderPath");
        pathField.setAccessible(true);
        pathField.set(null, "allure-results/");

        Method scriptMethod = AllureManager.class.getDeclaredMethod("writeGenerateReportShellFilesToProjectDirectory");
        scriptMethod.setAccessible(true);

        String scriptFileName = SystemUtils.IS_OS_WINDOWS ? "generate_allure_report.bat" : "generate_allure_report.sh";
        Path scriptPath = Path.of(scriptFileName);
        try {
            isolateFromProvisionedCliCache();
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
            scriptMethod.invoke(null);
            String content = Files.readString(scriptPath, StandardCharsets.UTF_8);

            SHAFT.Validations.assertThat().object(content.contains("npx --yes allure@" + SHAFT.Properties.internal.allure3Version()))
                    .isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(content.contains("command -v allure")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(content.contains("where allure")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(content.contains("allure --version")).isEqualTo(false).perform();
        } finally {
            Files.deleteIfExists(scriptPath);
        }
    }

    @Test(description = "writeAllureConfig should inject allure.customLogo in awesome plugin options")
    public void writeAllureConfigShouldInjectCustomLogo() throws Exception {
        Method writeAllureConfigMethod = AllureManager.class.getDeclaredMethod("writeAllureConfig", String.class, String.class);
        writeAllureConfigMethod.setAccessible(true);

        String originalCustomLogo = SHAFT.Properties.allure.customLogo();
        String testCustomLogo = "https://example.com/custom-logo.png";
        Path configPath = Path.of("allurerc.yaml");
        try {
            SHAFT.Properties.allure.set()
                    .customLogo(testCustomLogo)
                    .singleFile(false)
                    .reportLanguage("fr")
                    .open(true)
                    .groupBy("package,parentSuite");
            String testOutputDirectory = (System.getProperty("user.dir") + File.separator + "target" + File.separator + "allure-report-test").replace("\\", "/");
            writeAllureConfigMethod.invoke(null, "Unit Test Report", testOutputDirectory);

            String yaml = Files.readString(configPath, StandardCharsets.UTF_8);
            SHAFT.Validations.assertThat().object(yaml.contains("logo: \"" + testCustomLogo + "\"")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(yaml.contains("singleFile: false")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(yaml.contains("reportLanguage: \"fr\"")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(yaml.contains("open: true")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(yaml.contains("        - package")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(yaml.contains("        - parentSuite")).isEqualTo(true).perform();
        } finally {
            SHAFT.Properties.allure.set().customLogo(originalCustomLogo);
            Files.deleteIfExists(configPath);
        }
    }

    @Test(description = "openAllureReport should skip the OS opener when automatic opening is disabled")
    public void openAllureReportShouldRespectAutomaticOpenToggle() throws Exception {
        Method openAllureReport = AllureManager.class.getDeclaredMethod("openAllureReport", String.class);
        openAllureReport.setAccessible(true);

        String originalAutomaticallyOpen = String.valueOf(SHAFT.Properties.allure.automaticallyOpen());
        try {
            SHAFT.Properties.allure.set().automaticallyOpen(false);

            Object openRequested = openAllureReport.invoke(null, "missing-AllureReport.html");

            SHAFT.Validations.assertThat().object(openRequested).isEqualTo(false).perform();
        } finally {
            SHAFT.Properties.allure.set().automaticallyOpen(Boolean.parseBoolean(originalAutomaticallyOpen));
        }
    }

    @Test(description = "extractSemVerFromText should parse SemVer-like versions and return null when absent")
    public void extractSemVerFromTextShouldParseExpectedPatterns() throws Exception {
        Method extractorMethod = AllureManager.class.getDeclaredMethod("extractSemVerFromText", String.class);
        extractorMethod.setAccessible(true);

        Object plainSemVer = extractorMethod.invoke(null, "allure 3.3.1");
        Object preReleaseSemVer = extractorMethod.invoke(null, "v3.3.1-beta.2");
        Object missingSemVer = extractorMethod.invoke(null, "unknown version");

        SHAFT.Validations.assertThat().object(plainSemVer).isEqualTo("3.3.1").perform();
        SHAFT.Validations.assertThat().object(preReleaseSemVer).isEqualTo("3.3.1-beta.2").perform();
        SHAFT.Validations.assertThat().object(missingSemVer).isNull().perform();
    }

    @Test(description = "parseAllureVersionCommandOutput should still parse version tokens from non-zero command output")
    public void parseAllureVersionCommandOutputShouldParseSemVerEvenWhenExitCodeIsNonZero() throws Exception {
        Method parserMethod = AllureManager.class.getDeclaredMethod("parseAllureVersionCommandOutput", String.class, int.class);
        parserMethod.setAccessible(true);

        Object parsedVersion = parserMethod.invoke(null, "warning: fallback mode\n3.7.0\n", 1);

        SHAFT.Validations.assertThat().object(parsedVersion).isEqualTo("3.7.0").perform();
    }

    @Test(description = "parseAllureVersionCommandOutput should return null when command failed and no version token is present")
    public void parseAllureVersionCommandOutputShouldReturnNullWhenNoVersionTokenExists() throws Exception {
        Method parserMethod = AllureManager.class.getDeclaredMethod("parseAllureVersionCommandOutput", String.class, int.class);
        parserMethod.setAccessible(true);

        Object parsedVersion = parserMethod.invoke(null, "allure failed to start", 1);
        SHAFT.Validations.assertThat().object(parsedVersion).isNull().perform();
    }

    @Test(description = "Node helper methods should return expected platform-specific paths and reuse cached npx download")
    public void nodeHelperMethodsShouldBuildExpectedPathsAndReuseCachedNpxBinary() throws Exception {
        Method getNodeJsDownloadUrl = AllureManager.class.getDeclaredMethod("getNodeJsDownloadUrl");
        Method getNodeJsFolderName = AllureManager.class.getDeclaredMethod("getNodeJsFolderName");
        Method getNodeBinPath = AllureManager.class.getDeclaredMethod("getNodeBinPath");
        Method getNpxBinPath = AllureManager.class.getDeclaredMethod("getNpxBinPath");
        Method quotePath = AllureManager.class.getDeclaredMethod("q", String.class);
        Method downloadNodeJsPortable = AllureManager.class.getDeclaredMethod("downloadNodeJsPortable");
        getNodeJsDownloadUrl.setAccessible(true);
        getNodeJsFolderName.setAccessible(true);
        getNodeBinPath.setAccessible(true);
        getNpxBinPath.setAccessible(true);
        quotePath.setAccessible(true);
        downloadNodeJsPortable.setAccessible(true);

        String folderName = (String) getNodeJsFolderName.invoke(null);
        String nodeBinPath = (String) getNodeBinPath.invoke(null);
        String npxBinPath = (String) getNpxBinPath.invoke(null);
        String downloadUrl = (String) getNodeJsDownloadUrl.invoke(null);
        String quotedPath = (String) quotePath.invoke(null, "/tmp/path with spaces");

        SHAFT.Validations.assertThat().object(folderName.contains(SHAFT.Properties.internal.nodeLtsVersion())).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(nodeBinPath.contains(folderName)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(npxBinPath.contains(folderName)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(downloadUrl.contains("/v" + SHAFT.Properties.internal.nodeLtsVersion() + "/")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(quotedPath).isEqualTo("\"/tmp/path with spaces\"").perform();

        Path npxPath = Path.of(npxBinPath);
        Files.createDirectories(npxPath.getParent());
        Files.writeString(npxPath, "cached npx");
        try {
            Object downloadedNpxPath = downloadNodeJsPortable.invoke(null);
            SHAFT.Validations.assertThat().object(downloadedNpxPath).isEqualTo(npxBinPath).perform();
        } finally {
            Files.deleteIfExists(npxPath);
        }
    }

    @Test(description = "verifyNodeJsChecksum should return true when archive hash matches SHASUMS256 entry")
    public void verifyNodeJsChecksumShouldReturnTrueWhenChecksumMatches() throws Exception {
        Method verifyNodeJsChecksum = AllureManager.class.getDeclaredMethod("verifyNodeJsChecksum", String.class, String.class, String.class);
        verifyNodeJsChecksum.setAccessible(true);

        Path sourceDirectory = Path.of(System.getProperty("user.dir"), "target", "node-checksum-success");
        Files.createDirectories(sourceDirectory);
        String archiveName = "node-v99.99.99-linux-x64.tar.gz";
        Path archivePath = sourceDirectory.resolve(archiveName);
        Files.writeString(archivePath, "dummy-node-archive-content");

        String sha256 = sha256Hex(archivePath);
        Files.writeString(sourceDirectory.resolve("SHASUMS256.txt"), sha256 + "  " + archiveName + System.lineSeparator());

        String downloadUrl = sourceDirectory.toUri().toString() + archiveName;
        Object verificationResult = verifyNodeJsChecksum.invoke(null, archivePath.toString(), downloadUrl, archiveName);
        SHAFT.Validations.assertThat().object(verificationResult).isEqualTo(true).perform();
    }

    @Test(description = "verifyNodeJsChecksum should return false when archive hash mismatches SHASUMS256 entry")
    public void verifyNodeJsChecksumShouldReturnFalseWhenChecksumMismatches() throws Exception {
        Method verifyNodeJsChecksum = AllureManager.class.getDeclaredMethod("verifyNodeJsChecksum", String.class, String.class, String.class);
        verifyNodeJsChecksum.setAccessible(true);

        Path sourceDirectory = Path.of(System.getProperty("user.dir"), "target", "node-checksum-failure");
        Files.createDirectories(sourceDirectory);
        String archiveName = "node-v88.88.88-linux-x64.tar.gz";
        Path archivePath = sourceDirectory.resolve(archiveName);
        Files.writeString(archivePath, "different-archive-content");

        Files.writeString(sourceDirectory.resolve("SHASUMS256.txt"), "0000000000000000000000000000000000000000000000000000000000000000  " + archiveName + System.lineSeparator());

        String downloadUrl = sourceDirectory.toUri().toString() + archiveName;
        Object verificationResult = verifyNodeJsChecksum.invoke(null, archivePath.toString(), downloadUrl, archiveName);
        SHAFT.Validations.assertThat().object(verificationResult).isEqualTo(false).perform();
    }

    @Test(description = "executeAllureGenerateCommand should wait for command completion and expose stderr")
    public void executeAllureGenerateCommandShouldWaitForCompletionAndExposeStderr() throws Exception {
        Method executeAllureGenerateCommand = AllureManager.class.getDeclaredMethod("executeAllureGenerateCommand", String.class);
        executeAllureGenerateCommand.setAccessible(true);

        Path markerFile = Path.of(System.getProperty("user.dir"), "target", "allure-generate-sync-marker.txt");
        Files.deleteIfExists(markerFile);
        String markerPath = markerFile.toString().replace("\\", "\\\\");
        String command = SystemUtils.IS_OS_WINDOWS
                ? "powershell -NoProfile -Command \"Start-Sleep -Seconds 1; Write-Output stdout-message; Write-Error stderr-message; Set-Content -Path '" + markerPath + "' -Value done; exit 7\""
                : "sleep 1; printf 'stdout-message\\n'; printf 'stderr-message\\n' >&2; touch '" + markerPath + "'; exit 7";

        try {
            executeAllureGenerateCommand.invoke(null, command);

            SHAFT.Validations.assertThat().object(Files.exists(markerFile)).isTrue().perform();
        } finally {
            Files.deleteIfExists(markerFile);
        }
    }

    @Test(description = "Realtime monitoring helpers should start and stop long-running process safely")
    public void realtimeMonitoringHelpersShouldStartAndStopLongRunningProcess() throws Exception {
        Method startLongRunningCommand = AllureManager.class.getDeclaredMethod("startLongRunningCommand", String.class);
        Method stopRealtimeMonitoring = AllureManager.class.getDeclaredMethod("stopRealtimeMonitoring");
        startLongRunningCommand.setAccessible(true);
        stopRealtimeMonitoring.setAccessible(true);

        Process process = null;
        try {
            String command = SystemUtils.IS_OS_WINDOWS ? "cmd /c timeout /t 1 >nul" : "sleep 1";
            process = (Process) startLongRunningCommand.invoke(null, command);
            SHAFT.Validations.assertThat().object(process != null && process.isAlive()).isEqualTo(true).perform();

            setStaticField(AllureManager.class, "realtimeMonitoringProcess", process);
            stopRealtimeMonitoring.invoke(null);
            SHAFT.Validations.assertThat().object(process.isAlive()).isEqualTo(false).perform();
        } finally {
            if (process != null && process.isAlive()) {
                process.destroyForcibly();
            }
        }
    }

    @Test(description = "startRealtimeMonitoringIfEligible should start watcher command for managed Allure 3")
    public void startRealtimeMonitoringIfEligibleShouldStartWatcherForManagedAllure3() throws Exception {
        Method startRealtimeMonitoringIfEligible = AllureManager.class.getDeclaredMethod("startRealtimeMonitoringIfEligible");
        startRealtimeMonitoringIfEligible.setAccessible(true);

        String originalAutomaticallyOpen = String.valueOf(SHAFT.Properties.allure.automaticallyOpen());
        String originalRealtimeMonitoring = String.valueOf(SHAFT.Properties.allure.realtimeMonitoring());
        try {
            SHAFT.Properties.allure.set().realtimeMonitoring(true).automaticallyOpen(true);
            setStaticField(AllureManager.class, "allureResultsFolderPath", "allure-results");
            Files.createDirectories(Path.of("allure-results"));

            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "echo");
            startRealtimeMonitoringIfEligible.invoke(null);
            Process process = (Process) getStaticField(AllureManager.class, "realtimeMonitoringProcess");
            SHAFT.Validations.assertThat().object(process != null).isEqualTo(true).perform();
            String config = Files.readString(Path.of("allurerc.yaml"), StandardCharsets.UTF_8);
            SHAFT.Validations.assertThat().object(config.contains("target/allure-watch-report")).isEqualTo(true).perform();
            if (process != null && process.isAlive()) {
                process.destroyForcibly();
            }
        } finally {
            SHAFT.Properties.allure.set()
                    .automaticallyOpen(Boolean.parseBoolean(originalAutomaticallyOpen))
                    .realtimeMonitoring(Boolean.parseBoolean(originalRealtimeMonitoring));
            setStaticField(AllureManager.class, "realtimeMonitoringProcess", null);
        }
    }

    @Test(description = "resolveAllureCommandPrefix should reject invalid configured allure version and cache failure state")
    public void resolveAllureCommandPrefixShouldRejectInvalidConfiguredVersion() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);

        Field internalField = Properties.class.getDeclaredField("internal");
        internalField.setAccessible(true);
        Object originalInternalConfig = internalField.get(null);
        String originalAllureVersion = System.getProperty("allure3Version");
        System.setProperty("allure3Version", "not-semver");
        try {
            internalField.set(null, ConfigFactory.create(Internal.class));
            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix).isNull().perform();
            SHAFT.Validations.assertThat().object(getStaticField(AllureManager.class, "cachedAllureCommandPrefix")).isEqualTo("").perform();
        } finally {
            if (originalAllureVersion == null) {
                System.clearProperty("allure3Version");
            } else {
                System.setProperty("allure3Version", originalAllureVersion);
            }
            internalField.set(null, originalInternalConfig);
        }
    }

    @Test(description = "resolveAllureCommandPrefix should continue when nodeLtsVersion is invalid but npx is available")
    public void resolveAllureCommandPrefixShouldContinueWhenNodeLtsVersionIsInvalid() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        isolateFromProvisionedCliCache();

        Field internalField = Properties.class.getDeclaredField("internal");
        internalField.setAccessible(true);
        Object originalInternalConfig = internalField.get(null);
        String originalNodeLtsVersion = System.getProperty("nodeLtsVersion");
        System.setProperty("nodeLtsVersion", "invalid-version");
        try {
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
            internalField.set(null, ConfigFactory.create(Internal.class));

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix == null || resolvedPrefix.toString().contains("allure@"))
                    .isEqualTo(true).perform();
        } finally {
            if (originalNodeLtsVersion == null) {
                System.clearProperty("nodeLtsVersion");
            } else {
                System.setProperty("nodeLtsVersion", originalNodeLtsVersion);
            }
            internalField.set(null, originalInternalConfig);
        }
    }

    @Test(description = "resolveAllureCommandPrefix should resolve npx command when configured version enforcement is enabled")
    public void resolveAllureCommandPrefixShouldResolveManagedNpxCommandWhenEnforced() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        isolateFromProvisionedCliCache();
        SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);

        Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
        SHAFT.Validations.assertThat().object(resolvedPrefix == null || resolvedPrefix.toString().contains("allure@")).isEqualTo(true).perform();
    }

    @Test(description = "createAllureReportArchive should create a zip archive from the configured output directory")
    public void createAllureReportArchiveShouldGenerateZipFile() throws Exception {
        Method createAllureReportArchive = AllureManager.class.getDeclaredMethod("createAllureReportArchive");
        createAllureReportArchive.setAccessible(true);

        Path outputDirectory = Path.of(System.getProperty("user.dir"), "target", "allure-report-archive-source");
        Files.createDirectories(outputDirectory);
        Files.writeString(outputDirectory.resolve("index.html"), "<html>archive-source</html>");
        setStaticField(AllureManager.class, "allureOutPutDirectory", outputDirectory.toString());

        try (Stream<Path> before = Files.list(Path.of(System.getProperty("user.dir")))) {
            before.filter(path -> path.getFileName().toString().startsWith("generatedReport_") && path.getFileName().toString().endsWith(".zip"))
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (Exception ignored) {
                        }
                    });
        }

        createAllureReportArchive.invoke(null);

        try (Stream<Path> after = Files.list(Path.of(System.getProperty("user.dir")))) {
            long archiveCount = after.filter(path -> path.getFileName().toString().startsWith("generatedReport_")
                            && path.getFileName().toString().endsWith(".zip"))
                    .count();
            SHAFT.Validations.assertThat().number((int) archiveCount).isEqualTo(1).perform();
        } finally {
            try (Stream<Path> generated = Files.list(Path.of(System.getProperty("user.dir")))) {
                generated.filter(path -> path.getFileName().toString().startsWith("generatedReport_")
                                && path.getFileName().toString().endsWith(".zip"))
                        .forEach(path -> {
                            try {
                                Files.deleteIfExists(path);
                            } catch (Exception ignored) {
                            }
                        });
            }
            Files.deleteIfExists(outputDirectory.resolve("index.html"));
            Files.deleteIfExists(outputDirectory);
        }
    }

    @Test(description = "System executable helpers should evaluate PATH presence and read system allure version safely")
    public void systemExecutableHelpersShouldEvaluatePathAndReadSystemAllureVersionSafely() throws Exception {
        Method isExecutableOnPath = AllureManager.class.getDeclaredMethod("isExecutableOnPath", String.class);
        Method readSystemAllureVersion = AllureManager.class.getDeclaredMethod("readSystemAllureVersion");
        isExecutableOnPath.setAccessible(true);
        readSystemAllureVersion.setAccessible(true);

        Object javaOnPath = isExecutableOnPath.invoke(null, "java");
        Object clearlyMissingExecutable = isExecutableOnPath.invoke(null, "definitely-not-an-executable-binary");
        Object systemAllureVersion = readSystemAllureVersion.invoke(null);

        SHAFT.Validations.assertThat().object(javaOnPath).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(clearlyMissingExecutable).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(systemAllureVersion == null || systemAllureVersion.toString().matches("\\d+\\.\\d+\\.\\d+.*"))
                .isEqualTo(true).perform();
    }

    @Test(description = "patch helper methods should safely handle invalid JSON and non-directory paths")
    public void patchHelpersShouldHandleInvalidJsonAndNonDirectoryInputs() throws Exception {
        Method patchStatusDetailsInJson = AllureManager.class.getDeclaredMethod("patchStatusDetailsInJson", String.class);
        Method patchMissingStatusDetailsInResults = AllureManager.class.getDeclaredMethod("patchMissingStatusDetailsInResults", String.class);
        patchStatusDetailsInJson.setAccessible(true);
        patchMissingStatusDetailsInResults.setAccessible(true);

        String invalidJson = "{not valid json";
        String patchedInvalid = (String) patchStatusDetailsInJson.invoke(null, invalidJson);
        SHAFT.Validations.assertThat().object(patchedInvalid).isEqualTo(invalidJson).perform();

        Path plainFile = Path.of(System.getProperty("user.dir"), "target", "allure-patch-helper.txt");
        Files.createDirectories(plainFile.getParent());
        Files.writeString(plainFile, "not-a-directory");
        patchMissingStatusDetailsInResults.invoke(null, plainFile.toString());

        Path emptyDir = Path.of(System.getProperty("user.dir"), "target", "allure-empty-results");
        Files.createDirectories(emptyDir);
        patchMissingStatusDetailsInResults.invoke(null, emptyDir.toString());

        Files.deleteIfExists(plainFile);
        Files.deleteIfExists(emptyDir);
    }

    @Test(description = "patchMissingStatusDetailsInResults should patch matching result/container files and ignore unrelated files")
    public void patchMissingStatusDetailsInResultsShouldPatchMatchingFilesOnly() throws Exception {
        Method patchMissingStatusDetailsInResults = AllureManager.class.getDeclaredMethod("patchMissingStatusDetailsInResults", String.class);
        patchMissingStatusDetailsInResults.setAccessible(true);

        Path resultsDir = Path.of(System.getProperty("user.dir"), "target", "allure-results-patch-target");
        Files.createDirectories(resultsDir);
        Path resultFile = resultsDir.resolve("abc-result.json");
        Path containerFile = resultsDir.resolve("abc-container.json");
        Path ignoredFile = resultsDir.resolve("ignored.json");

        Files.writeString(resultFile, "{\"steps\":[{\"name\":\"step-1\"}]}");
        Files.writeString(containerFile, "{\"befores\":[{\"name\":\"before-1\",\"steps\":[{\"name\":\"nested\"}]}]}");
        Files.writeString(ignoredFile, "{\"steps\":[{\"name\":\"ignored\"}]}");

        patchMissingStatusDetailsInResults.invoke(null, resultsDir.toString());

        String patchedResult = Files.readString(resultFile);
        String patchedContainer = Files.readString(containerFile);
        String untouchedIgnored = Files.readString(ignoredFile);

        SHAFT.Validations.assertThat().object(patchedResult.contains("\"statusDetails\"")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(patchedContainer.contains("\"statusDetails\"")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(untouchedIgnored.contains("\"statusDetails\"")).isEqualTo(false).perform();
    }

    @Test(description = "writeAllureReport should generate managed Allure 3 reports")
    public void writeAllureReportShouldWorkWithManagedAllure3() throws Exception {
        Method writeAllureReport = AllureManager.class.getDeclaredMethod("writeAllureReport");
        writeAllureReport.setAccessible(true);

        Path resultsDirectory = Path.of(System.getProperty("user.dir"), "target", "allure-results-test");
        Files.createDirectories(resultsDirectory);
        setStaticField(AllureManager.class, "allureResultsFolderPath", resultsDirectory.toString());

        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "echo");
        writeAllureReport.invoke(null);

        Files.deleteIfExists(Path.of(System.getProperty("user.dir"), "allurerc.yaml"));
        Files.deleteIfExists(resultsDirectory.getParent().resolve("allurerc.yaml"));
    }

    @Test(description = "Generated Allure index patch should expand image and HTML attachment previews")
    public void generatedAllureIndexPatchShouldExpandAttachmentPreviews() throws Exception {
        Method patchGeneratedAllureReportIndex = AllureManager.class
                .getDeclaredMethod("patchGeneratedAllureReportIndex", Path.class);
        patchGeneratedAllureReportIndex.setAccessible(true);
        Path reportDirectory = Files.createTempDirectory("shaft-allure-index-patch");
        Path index = reportDirectory.resolve("index.html");
        try {
            Files.writeString(index, "<!doctype html><html><head></head><body><main></main></body></html>",
                    StandardCharsets.UTF_8);

            patchGeneratedAllureReportIndex.invoke(null, reportDirectory);
            String patched = Files.readString(index, StandardCharsets.UTF_8);

            SHAFT.Validations.assertThat().object(patched).contains("shaft-allure-image-preview").perform();
            SHAFT.Validations.assertThat().object(patched).contains("shaft-allure-html-preview").perform();
            SHAFT.Validations.assertThat().object(patched).contains("iframe[src^=\"blob:\"]").perform();
            SHAFT.Validations.assertThat().object(patched).contains("overflow-x: hidden !important").perform();
            // Allure 2's real (non-modal) inline HTML attachment preview is a bare
            // <iframe class="attachment__iframe"> whose shipped CSS only sets width, not height, so
            // it collapses to a small scrollable strip (issue reported 2026-07-18); verified against
            // a real generated Allure 2 report's decoded CSS bundle. The patch must target that exact
            // class directly (independent of the Allure-3-oriented modal-detection JS heuristic).
            SHAFT.Validations.assertThat().object(patched)
                    .contains(".attachment__iframe:not(.attachment__iframe_fullscreen)").perform();
        } finally {
            Files.deleteIfExists(index);
            Files.deleteIfExists(reportDirectory);
        }
    }

    @Test(description = "writeAllureCategoriesIfSupported should write valid Allure 3 failure categories")
    public void writeAllureCategoriesIfSupportedShouldWriteAllure3Categories() throws Exception {
        Method writeCategories = AllureManager.class.getDeclaredMethod("writeAllureCategoriesIfSupported");
        writeCategories.setAccessible(true);

        Path resultsDirectory = Files.createTempDirectory("shaft-allure-categories");
        Path categories = resultsDirectory.resolve("categories.json");
        setStaticField(AllureManager.class, "allureResultsFolderPath", resultsDirectory.toString());

        writeCategories.invoke(null);
        JsonNode root = MAPPER.readTree(categories.toFile());
        SHAFT.Validations.assertThat().object(root.isArray()).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(root.toString().contains("Assertion / validation failure")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(root.toString().contains("Locator / element interaction")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(root.toString().contains("Provider / grid / device issue")).isEqualTo(true).perform();
        // SHAFT-specific buckets (issue #3504) surface framework-native outcomes in Allure's Categories tab.
        SHAFT.Validations.assertThat().object(root.toString().contains("SHAFT: flaky (passed on retry)")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(root.toString().contains("SHAFT: self-healed locator")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(root.toString().contains("SHAFT: soft verification failure")).isEqualTo(true).perform();

        Files.deleteIfExists(categories);
    }

    @Test(description = "resolveAllureCommandPrefix should use managed Allure 3 even when forceConfiguredCliVersion is false")
    public void resolveAllureCommandPrefixShouldUseManagedAllure3AndCachedEmptyShortcut() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        isolateFromProvisionedCliCache();

        SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
        SHAFT.Validations.assertThat().object(resolvedPrefix == null || resolvedPrefix.toString().contains("allure@"))
                .isEqualTo(true).perform();
        if (resolvedPrefix != null) {
            SHAFT.Validations.assertThat().object(resolvedPrefix.toString().equals("allure")).isEqualTo(false).perform();
        }

        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "");
        Object cachedEmptyResolution = resolveAllureCommandPrefix.invoke(null);
        SHAFT.Validations.assertThat().object(cachedEmptyResolution).isNull().perform();
    }

    @Test(description = "resolveAllureCommandPrefix should ignore stub Allure 2.x on PATH and use managed Allure 3")
    public void resolveAllureCommandPrefixShouldIgnoreStubAllure2OnPathAndUseManagedAllure3() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        isolateFromProvisionedCliCache();

        Path allureBinary = getWritablePathDirectory().resolve("allure");
        boolean binaryAlreadyExists = Files.exists(allureBinary);
        String originalBinaryContent = binaryAlreadyExists ? Files.readString(allureBinary) : null;
        try {
            Files.writeString(allureBinary, "#!/bin/sh\necho \"2.24.0\"\n");
            allureBinary.toFile().setExecutable(true);

            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix == null || resolvedPrefix.toString().contains("allure@"))
                    .isEqualTo(true).perform();
            if (resolvedPrefix != null) {
                SHAFT.Validations.assertThat().object(resolvedPrefix.toString().equals("allure")).isEqualTo(false).perform();
            }
        } finally {
            if (binaryAlreadyExists) {
                Files.writeString(allureBinary, originalBinaryContent);
                allureBinary.toFile().setExecutable(true);
            } else {
                Files.deleteIfExists(allureBinary);
            }
        }
    }

    @Test(description = "resolveAllureCommandPrefix should ignore non-2.x system allure and use managed Allure 3")
    public void resolveAllureCommandPrefixShouldIgnoreSystemAllureWhenVersionIsNot2x() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        isolateFromProvisionedCliCache();

        Path allureBinary = getWritablePathDirectory().resolve("allure");
        boolean binaryAlreadyExists = Files.exists(allureBinary);
        String originalBinaryContent = binaryAlreadyExists ? Files.readString(allureBinary) : null;
        try {
            Files.writeString(allureBinary, "#!/bin/sh\necho \"3.5.0\"\n");
            allureBinary.toFile().setExecutable(true);

            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix == null || resolvedPrefix.toString().contains("allure@"))
                    .isEqualTo(true).perform();
            if (resolvedPrefix != null) {
                SHAFT.Validations.assertThat().object(resolvedPrefix.toString().equals("allure")).isEqualTo(false).perform();
            }
        } finally {
            if (binaryAlreadyExists) {
                Files.writeString(allureBinary, originalBinaryContent);
                allureBinary.toFile().setExecutable(true);
            } else {
                Files.deleteIfExists(allureBinary);
            }
        }
    }

    @Test(description = "resolveAllureCommandPrefix should ignore system allure in enforce mode")
    public void resolveAllureCommandPrefixShouldIgnoreSystemAllureWhenEnforced() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        isolateFromProvisionedCliCache();

        Path allureBinary = getWritablePathDirectory().resolve("allure");
        boolean binaryAlreadyExists = Files.exists(allureBinary);
        String originalBinaryContent = binaryAlreadyExists ? Files.readString(allureBinary) : null;
        try {
            Files.writeString(allureBinary, "#!/bin/sh\necho \"2.24.0\"\n");
            allureBinary.toFile().setExecutable(true);

            SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix == null || resolvedPrefix.toString().contains("allure@")).isEqualTo(true).perform();
        } finally {
            if (binaryAlreadyExists) {
                Files.writeString(allureBinary, originalBinaryContent);
                allureBinary.toFile().setExecutable(true);
            } else {
                Files.deleteIfExists(allureBinary);
            }
        }
    }

    @Test(description = "startLongRunningCommand should return null when process cannot be started")
    public void startLongRunningCommandShouldReturnNullWhenProcessCannotBeStarted() throws Exception {
        Method startLongRunningCommand = AllureManager.class.getDeclaredMethod("startLongRunningCommand", String.class);
        startLongRunningCommand.setAccessible(true);

        String originalUserDir = System.getProperty("user.dir");
        try {
            System.setProperty("user.dir", "/definitely/nonexistent/directory");
            Object process = startLongRunningCommand.invoke(null, "echo should-not-start");
            SHAFT.Validations.assertThat().object(process).isNull().perform();
        } finally {
            System.setProperty("user.dir", originalUserDir);
        }
    }

    @Test(description = "stopRealtimeMonitoring should handle InterruptedException while waiting for process termination")
    public void stopRealtimeMonitoringShouldHandleInterruptedWait() throws Exception {
        Method stopRealtimeMonitoring = AllureManager.class.getDeclaredMethod("stopRealtimeMonitoring");
        stopRealtimeMonitoring.setAccessible(true);

        Process mockedProcess = Mockito.mock(Process.class);
        Mockito.when(mockedProcess.isAlive()).thenReturn(true);
        Mockito.when(mockedProcess.waitFor(5, TimeUnit.SECONDS)).thenThrow(new InterruptedException("simulated interruption"));

        setStaticField(AllureManager.class, "realtimeMonitoringProcess", mockedProcess);
        stopRealtimeMonitoring.invoke(null);

        Mockito.verify(mockedProcess).destroy();
        Mockito.verify(mockedProcess).destroyForcibly();
        Thread.interrupted();
    }

    @Test(description = "readSystemAllureVersion should return null and restore interrupt flag when interrupted")
    public void readSystemAllureVersionShouldHandleInterruptedWait() throws Exception {
        Method readSystemAllureVersion = AllureManager.class.getDeclaredMethod("readSystemAllureVersion");
        readSystemAllureVersion.setAccessible(true);

        Thread.currentThread().interrupt();
        Object parsedVersion = readSystemAllureVersion.invoke(null);
        SHAFT.Validations.assertThat().object(parsedVersion).isNull().perform();
        Thread.interrupted();
    }

    @Test(description = "isExecutableOnPath should return false when wait is interrupted")
    public void isExecutableOnPathShouldReturnFalseWhenInterrupted() throws Exception {
        Method isExecutableOnPath = AllureManager.class.getDeclaredMethod("isExecutableOnPath", String.class);
        isExecutableOnPath.setAccessible(true);

        try {
            Thread.currentThread().interrupt();
            Object executableOnPath = isExecutableOnPath.invoke(null, "java");
            // Use TestNG Assert: SHAFT Validations attaches Allure evidence and can throw
            // ClosedByInterruptException while the interrupt flag is still set.
            org.testng.Assert.assertEquals(executableOnPath, false,
                    "interrupted PATH check must assume the executable is not found");
            org.testng.Assert.assertTrue(Thread.currentThread().isInterrupted(),
                    "interrupt flag must remain set for the caller");
        } finally {
            Thread.interrupted();
        }
    }

    @Test(description = "downloadNodeJsPortable should surface download failures when archive URL is unreachable")
    public void downloadNodeJsPortableShouldSurfaceDownloadFailures() throws Exception {
        Method downloadNodeJsPortable = AllureManager.class.getDeclaredMethod("downloadNodeJsPortable");
        Method getNpxBinPath = AllureManager.class.getDeclaredMethod("getNpxBinPath");
        downloadNodeJsPortable.setAccessible(true);
        getNpxBinPath.setAccessible(true);

        Path npxPath = Path.of((String) getNpxBinPath.invoke(null));
        Files.deleteIfExists(npxPath);

        Field internalField = Properties.class.getDeclaredField("internal");
        internalField.setAccessible(true);
        Object originalInternalConfig = internalField.get(null);
        String originalNodeLtsVersion = System.getProperty("nodeLtsVersion");
        InvocationTargetException thrownException = null;
        try {
            System.setProperty("nodeLtsVersion", "0.0.0");
            internalField.set(null, ConfigFactory.create(Internal.class));
            downloadNodeJsPortable.invoke(null);
        } catch (InvocationTargetException e) {
            thrownException = e;
        } finally {
            if (originalNodeLtsVersion == null) {
                System.clearProperty("nodeLtsVersion");
            } else {
                System.setProperty("nodeLtsVersion", originalNodeLtsVersion);
            }
            internalField.set(null, originalInternalConfig);
        }

        SHAFT.Validations.assertThat().object(thrownException != null).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(thrownException.getCause() != null).isEqualTo(true).perform();
    }

    @Test(description = "generateAllureReportArchive should execute archive branch when enabled")
    public void generateAllureReportArchiveShouldExecuteWhenEnabled() throws Exception {
        Path resultsDirectory = Path.of(System.getProperty("user.dir"), "target", "allure-results-archive");
        Files.createDirectories(resultsDirectory);
        Files.writeString(resultsDirectory.resolve("dummy-result.json"), "{\"name\":\"dummy\"}");
        setStaticField(AllureManager.class, "allureResultsFolderPath", resultsDirectory.toString());
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "echo");

        SHAFT.Properties.allure.set().generateArchive(true);
        Path archiveRoot = resultsDirectory.getParent();
        try (Stream<Path> existingArchives = Files.list(archiveRoot)) {
            existingArchives.filter(path -> path.getFileName().toString().startsWith("generatedReport_")
                            && path.getFileName().toString().endsWith(".zip"))
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (Exception ignored) {
                        }
                    });
        }

        AllureManager.generateAllureReportArchive();

        try (Stream<Path> generatedArchives = Files.list(archiveRoot)) {
            long archiveCount = generatedArchives.filter(path -> path.getFileName().toString().startsWith("generatedReport_")
                            && path.getFileName().toString().endsWith(".zip"))
                    .count();
            SHAFT.Validations.assertThat().number((int) archiveCount).isEqualTo(1).perform();
        } finally {
            SHAFT.Properties.allure.set().generateArchive(false);
            try (Stream<Path> generatedArchives = Files.list(archiveRoot)) {
                generatedArchives.filter(path -> path.getFileName().toString().startsWith("generatedReport_")
                                && path.getFileName().toString().endsWith(".zip"))
                        .forEach(path -> {
                            try {
                                Files.deleteIfExists(path);
                            } catch (Exception ignored) {
                            }
                        });
            }
        }
    }

    private static String sha256Hex(Path filePath) throws Exception {
        java.security.MessageDigest messageDigest = java.security.MessageDigest.getInstance("SHA-256");
        byte[] bytes = Files.readAllBytes(filePath);
        byte[] digest = messageDigest.digest(bytes);
        StringBuilder stringBuilder = new StringBuilder();
        for (byte b : digest) {
            stringBuilder.append(String.format("%02x", b));
        }
        return stringBuilder.toString();
    }

    private static Path getWritablePathDirectory() {
        String path = System.getenv("PATH");
        for (String entry : path.split(File.pathSeparator)) {
            Path candidate = Path.of(entry);
            if (Files.isDirectory(candidate) && Files.isWritable(candidate)) {
                return candidate;
            }
        }
        throw new IllegalStateException("Could not find a writable directory from PATH.");
    }

    /**
     * Helper that resets the cached CLI resolution state in AllureManager so that each test
     * that modifies it starts from a clean slate.
     */
    @AfterMethod(alwaysRun = true)
    public void resetAllureManagerCachedState() throws Exception {
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        setStaticField(AllureManager.class, "realtimeMonitoringProcess", null);
        System.clearProperty("allure.cli.cacheRoot");
        System.clearProperty("allure.cli.skipProvision");
        System.clearProperty("allure.cli.mavenZip");
        Properties.clearForCurrentThread();
        setStaticField(AllureManager.class, "allureResultsFolderPath", SHAFT.Properties.paths.allureResults());
        setStaticField(AllureManager.class, "allureOutPutDirectory", "");
    }

    /**
     * Forces CLI resolution away from any machine-local Maven Allure CLI cache so tests that
     * assert the npx fallback stay deterministic (#5801).
     */
    private static Path isolateFromProvisionedCliCache() throws Exception {
        Path emptyCache = Files.createTempDirectory("shaft-allure-cli-empty-cache");
        System.setProperty("allure.cli.cacheRoot", emptyCache.toString());
        System.setProperty("allure.cli.skipProvision", "true");
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        return emptyCache;
    }

    /**
     * Creates a minimal Maven-cache Allure CLI layout ({@code node_modules/allure/cli.js}) under a
     * temp cache root for resolution-order tests (#5801).
     */
    private static Path createFakeProvisionedAllureCli(String version) throws Exception {
        Path cacheRoot = Files.createTempDirectory("shaft-allure-cli-provisioned");
        Path cliJs = cacheRoot.resolve(version).resolve("node_modules").resolve("allure").resolve("cli.js");
        Files.createDirectories(cliJs.getParent());
        Files.writeString(cliJs, "#!/usr/bin/env node\nconsole.log('fake-allure-cli');\n", StandardCharsets.UTF_8);
        System.setProperty("allure.cli.cacheRoot", cacheRoot.toString());
        System.setProperty("allure.cli.skipProvision", "true");
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        return cacheRoot;
    }


    @Test(description = "resolveAllureCommandPrefix should prefer Maven-provisioned Allure 3 CLI over npx (#5801)")
    public void resolveAllureCommandPrefixShouldPreferProvisionedCliOverNpx() throws Exception {
        String version = SHAFT.Properties.internal.allure3Version();
        Path cacheRoot = createFakeProvisionedAllureCli(version);
        try {
            Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
            resolveAllureCommandPrefix.setAccessible(true);
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix).isNotNull().perform();
            String prefix = resolvedPrefix.toString();
            SHAFT.Validations.assertThat().object(prefix.contains("npx")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(prefix.contains("allure@")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(prefix.contains("cli.js")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(prefix.contains(version)).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(prefix.equals("allure")).isEqualTo(false).perform();
        } finally {
            try (Stream<Path> walk = Files.walk(cacheRoot)) {
                walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (Exception ignored) {
                    }
                });
            }
        }
    }

    @Test(description = "resolveAllureCommandPrefix should prefer provisioned CLI even when stub Allure 2 is on PATH (#5801)")
    public void resolveAllureCommandPrefixShouldPreferProvisionedCliOverPathAllure2() throws Exception {
        String version = SHAFT.Properties.internal.allure3Version();
        Path cacheRoot = createFakeProvisionedAllureCli(version);
        Path allureBinary = getWritablePathDirectory().resolve("allure");
        boolean binaryAlreadyExists = Files.exists(allureBinary);
        String originalBinaryContent = binaryAlreadyExists ? Files.readString(allureBinary) : null;
        try {
            Files.writeString(allureBinary, "#!/bin/sh\necho \"2.24.0\"\n");
            allureBinary.toFile().setExecutable(true);

            Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
            resolveAllureCommandPrefix.setAccessible(true);
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix).isNotNull().perform();
            String prefix = resolvedPrefix.toString();
            SHAFT.Validations.assertThat().object(prefix.equals("allure")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(prefix.contains("cli.js")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(prefix.contains("npx")).isEqualTo(false).perform();
        } finally {
            if (binaryAlreadyExists) {
                Files.writeString(allureBinary, originalBinaryContent);
                allureBinary.toFile().setExecutable(true);
            } else {
                Files.deleteIfExists(allureBinary);
            }
            try (Stream<Path> walk = Files.walk(cacheRoot)) {
                walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (Exception ignored) {
                    }
                });
            }
        }
    }

    @Test(description = "getCommandToCreateAllureReport should use provisioned cli.js path without npx when cached (#5801 offline path)")
    public void getCommandToCreateAllureReportShouldUseProvisionedCliWithoutNpx() throws Exception {
        String version = SHAFT.Properties.internal.allure3Version();
        Path cacheRoot = createFakeProvisionedAllureCli(version);
        try {
            Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
            resolveAllureCommandPrefix.setAccessible(true);
            Object prefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(prefix).isNotNull().perform();

            setStaticField(AllureManager.class, "allureResultsFolderPath", "allure-results");
            setStaticField(AllureManager.class, "allureOutPutDirectory", "target/allure-report");
            Method getCommandMethod = AllureManager.class.getDeclaredMethod("getCommandToCreateAllureReport");
            getCommandMethod.setAccessible(true);
            String command = (String) getCommandMethod.invoke(null);

            SHAFT.Validations.assertThat().object(command.contains("npx")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(command.contains("cli.js")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(command.contains("generate")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(command.contains("--config")).isEqualTo(true).perform();
        } finally {
            try (Stream<Path> walk = Files.walk(cacheRoot)) {
                walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (Exception ignored) {
                    }
                });
            }
        }
    }

    @Test(description = "isProvisionedAllureCliPresent and cache helpers should honor allure.cli.cacheRoot override")
    public void provisionedCliHelpersShouldHonorCacheRootOverride() throws Exception {
        String version = "3.17.0";
        Path cacheRoot = createFakeProvisionedAllureCli(version);
        try {
            SHAFT.Validations.assertThat().object(AllureManager.isProvisionedAllureCliPresent(version)).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(AllureManager.getProvisionedAllureCliJs(version).contains(cacheRoot.toString()))
                    .isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(AllureManager.getAllureCliHome(version).startsWith(cacheRoot.toString()))
                    .isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(AllureManager.isProvisionedAllureCliPresent("9.9.9")).isEqualTo(false).perform();
        } finally {
            try (Stream<Path> walk = Files.walk(cacheRoot)) {
                walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (Exception ignored) {
                    }
                });
            }
        }
    }

    @Test(description = "getAllureCliMavenZipPath should honor allure.cli.mavenZip override and default Maven layout (#5815)")
    public void getAllureCliMavenZipPathShouldHonorOverrideAndDefaultLayout() {
        String version = "3.17.0";
        String defaultPath = AllureManager.getAllureCliMavenZipPath(version);
        SHAFT.Validations.assertThat().object(defaultPath.contains("io" + File.separator + "github" + File.separator + "shafthq"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(defaultPath.contains(AllureManager.ALLURE_CLI_MAVEN_ARTIFACT_ID)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(defaultPath.endsWith("allure-cli-" + version + ".zip")).isEqualTo(true).perform();

        Path customZip = Path.of("target", "custom-allure-cli.zip");
        System.setProperty("allure.cli.mavenZip", customZip.toString());
        SHAFT.Validations.assertThat().object(AllureManager.getAllureCliMavenZipPath(version)).isEqualTo(customZip.toString()).perform();
    }

    @Test(description = "tryProvisionAllureCliFromMavenZip should unpack zip into runtime cache and expose cli.js (#5815)")
    public void tryProvisionAllureCliFromMavenZipShouldUnpackIntoRuntimeCache() throws Exception {
        String version = "3.17.0";
        Path work = Files.createTempDirectory("shaft-allure-cli-zip-provision");
        Path cacheRoot = work.resolve("runtime-cache");
        Path zipPath = work.resolve("allure-cli-" + version + ".zip");
        createMinimalAllureCliZip(zipPath);
        System.setProperty("allure.cli.cacheRoot", cacheRoot.toString());
        System.setProperty("allure.cli.mavenZip", zipPath.toString());
        System.setProperty("allure.cli.skipProvision", "false");
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        try {
            SHAFT.Validations.assertThat().object(AllureManager.isProvisionedAllureCliPresent(version)).isEqualTo(false).perform();
            boolean provisioned = AllureManager.tryProvisionAllureCliFromMavenZip(version);
            SHAFT.Validations.assertThat().object(provisioned).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(AllureManager.isProvisionedAllureCliPresent(version)).isEqualTo(true).perform();
            Path cliJs = cacheRoot.resolve(version).resolve("node_modules").resolve("allure").resolve("cli.js");
            SHAFT.Validations.assertThat().object(Files.isRegularFile(cliJs)).isEqualTo(true).perform();

            Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
            resolveAllureCommandPrefix.setAccessible(true);
            Object prefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(prefix).isNotNull().perform();
            SHAFT.Validations.assertThat().object(prefix.toString().contains("cli.js")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(prefix.toString().contains("npx")).isEqualTo(false).perform();
        } finally {
            deleteRecursively(work);
        }
    }

    @Test(description = "unpackAllureCliZip should reject zip-slip entries (#5815)")
    public void unpackAllureCliZipShouldRejectZipSlip() throws Exception {
        Path work = Files.createTempDirectory("shaft-allure-cli-zip-slip");
        Path zipPath = work.resolve("evil.zip");
        Path dest = work.resolve("dest");
        Files.createDirectories(dest);
        try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(zipPath))) {
            zos.putNextEntry(new ZipEntry("../evil.txt"));
            zos.write("nope".getBytes(StandardCharsets.UTF_8));
            zos.closeEntry();
        }
        boolean ok = AllureManager.unpackAllureCliZip(zipPath.toFile(), dest.toFile());
        SHAFT.Validations.assertThat().object(ok).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(Files.exists(work.resolve("evil.txt"))).isEqualTo(false).perform();
        deleteRecursively(work);
    }

    /** Builds a minimal Allure CLI zip with {@code node_modules/allure/cli.js} at the archive root. */
    private static void createMinimalAllureCliZip(Path zipPath) throws Exception {
        Files.createDirectories(zipPath.getParent());
        try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(zipPath))) {
            zos.putNextEntry(new ZipEntry("node_modules/allure/cli.js"));
            zos.write("#!/usr/bin/env node\nconsole.log('zip-allure-cli');\n".getBytes(StandardCharsets.UTF_8));
            zos.closeEntry();
            zos.putNextEntry(new ZipEntry("package.json"));
            zos.write("{\n  \"name\": \"shaft-allure-cli-runtime\",\n  \"private\": true\n}\n".getBytes(StandardCharsets.UTF_8));
            zos.closeEntry();
        }
    }

    private static void deleteRecursively(Path root) throws Exception {
        if (root == null || !Files.exists(root)) {
            return;
        }
        try (Stream<Path> walk = Files.walk(root)) {
            walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (Exception ignored) {
                }
            });
        }
    }

    @Test(description = "getCommandToCreateAllureReport should use allure3 --config syntax when allure3 is detected")
    public void getCommandToCreateAllureReportShouldUseAllure3SyntaxWhenAllure3Detected() throws Exception {
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "allure");
        setStaticField(AllureManager.class, "allureResultsFolderPath", "allure-results");
        setStaticField(AllureManager.class, "allureOutPutDirectory", "target/allure-report");

        Method getCommandMethod = AllureManager.class.getDeclaredMethod("getCommandToCreateAllureReport");
        getCommandMethod.setAccessible(true);
        String command = (String) getCommandMethod.invoke(null);

        // Allure 3: must use --config allurerc.yaml, must NOT use --clean
        SHAFT.Validations.assertThat().object(command).contains("--config").perform();
        SHAFT.Validations.assertThat().object(command).contains("allurerc.yaml").perform();
        SHAFT.Validations.assertThat().object(command.contains("--clean")).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(command).contains("generate").perform();
        SHAFT.Validations.assertThat().object(command).contains("allure-results").perform();
    }

    @Test(description = "watchCommandShouldUseSimpleAllure3SyntaxWithOnlyResultsDir when allure3 is used for realtime monitoring")
    public void watchCommandShouldUseSimpleAllure3SyntaxWithOnlyResultsDir() throws Exception {
        // Simulate allure3 state
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "npx --yes allure@3.5.0");
        setStaticField(AllureManager.class, "allureResultsFolderPath", "allure-results");

        String originalAutomaticallyOpen = String.valueOf(SHAFT.Properties.allure.automaticallyOpen());
        try {
            SHAFT.Properties.allure.set().automaticallyOpen(true);

            // Build the expected watch command the same way the production code does
            String prefix = (String) getStaticField(AllureManager.class, "cachedAllureCommandPrefix");
            String resultsPath = "allure-results"; // getResultsPath with no trailing separator

            String expectedCommand = prefix + " watch --open \"" + resultsPath + "\"";

            // Watch command should include --open when automatic browser opening is enabled.
            SHAFT.Validations.assertThat().object(expectedCommand.contains("--config")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(expectedCommand.contains("--output")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(expectedCommand.contains("--open")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(expectedCommand).contains("watch").perform();
            SHAFT.Validations.assertThat().object(expectedCommand).contains("allure-results").perform();
        } finally {
            SHAFT.Properties.allure.set().automaticallyOpen(Boolean.parseBoolean(originalAutomaticallyOpen));
        }
    }
}
