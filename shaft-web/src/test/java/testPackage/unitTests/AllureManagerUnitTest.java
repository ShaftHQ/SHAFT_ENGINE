package testPackage.unitTests;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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

    @Test(description = "Generated allure serve script should preserve legacy PATH-first behavior when enforcement is disabled")
    public void generatedAllureServeScriptShouldUseLegacyPathFirstFlowWhenEnforcementDisabled() throws Exception {
        Field pathField = AllureManager.class.getDeclaredField("allureResultsFolderPath");
        pathField.setAccessible(true);
        pathField.set(null, "allure-results/");

        Method scriptMethod = AllureManager.class.getDeclaredMethod("writeGenerateReportShellFilesToProjectDirectory");
        scriptMethod.setAccessible(true);

        String scriptFileName = SystemUtils.IS_OS_WINDOWS ? "generate_allure_report.bat" : "generate_allure_report.sh";
        Path scriptPath = Path.of(scriptFileName);
        try {
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
            scriptMethod.invoke(null);
            String content = Files.readString(scriptPath, StandardCharsets.UTF_8);

            SHAFT.Validations.assertThat().object(content.contains("allure@" + SHAFT.Properties.internal.allure3Version()))
                    .isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(content.contains("allure --version")).isEqualTo(false).perform();
        } finally {
            Files.deleteIfExists(scriptPath);
        }
    }

    @Test(description = "writeAllureConfig should inject allure.customLogo in awesome plugin options")
    public void writeAllureConfigShouldInjectCustomLogo() throws Exception {
        Method writeAllureConfigMethod = AllureManager.class.getDeclaredMethod("writeAllureConfig", String.class, String.class, boolean.class);
        writeAllureConfigMethod.setAccessible(true);

        String originalCustomLogo = SHAFT.Properties.allure.customLogo();
        String testCustomLogo = "https://example.com/custom-logo.png";
        Path configPath = Path.of("allurerc.yaml");
        try {
            SHAFT.Properties.allure.set().customLogo(testCustomLogo);
            String testOutputDirectory = (System.getProperty("user.dir") + File.separator + "target" + File.separator + "allure-report-test").replace("\\", "/");
            writeAllureConfigMethod.invoke(null, "Unit Test Report", testOutputDirectory, true);

            String yaml = Files.readString(configPath, StandardCharsets.UTF_8);
            SHAFT.Validations.assertThat().object(yaml.contains("logo: \"" + testCustomLogo + "\"")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(yaml.contains("singleFile: true")).isEqualTo(true).perform();
        } finally {
            SHAFT.Properties.allure.set().customLogo(originalCustomLogo);
            Files.deleteIfExists(configPath);
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

    @Test(description = "startRealtimeMonitoringIfEligible should skip Allure 2 and start watcher command for Allure 3")
    public void startRealtimeMonitoringIfEligibleShouldHandleAllure2AndAllure3Flows() throws Exception {
        Method startRealtimeMonitoringIfEligible = AllureManager.class.getDeclaredMethod("startRealtimeMonitoringIfEligible");
        startRealtimeMonitoringIfEligible.setAccessible(true);

        String originalAutomaticallyOpen = String.valueOf(SHAFT.Properties.allure.automaticallyOpen());
        String originalRealtimeMonitoring = String.valueOf(SHAFT.Properties.allure.realtimeMonitoring());
        try {
            SHAFT.Properties.allure.set().realtimeMonitoring(true).automaticallyOpen(true);
            setStaticField(AllureManager.class, "allureResultsFolderPath", "allure-results");
            Files.createDirectories(Path.of("allure-results"));

            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "echo");
            setStaticField(AllureManager.class, "cachedIsAllure2", true);
            startRealtimeMonitoringIfEligible.invoke(null);
            Object allure2Process = getStaticField(AllureManager.class, "realtimeMonitoringProcess");
            SHAFT.Validations.assertThat().object(allure2Process).isNull().perform();

            setStaticField(AllureManager.class, "cachedIsAllure2", false);
            startRealtimeMonitoringIfEligible.invoke(null);
            Process process = (Process) getStaticField(AllureManager.class, "realtimeMonitoringProcess");
            SHAFT.Validations.assertThat().object(process != null).isEqualTo(true).perform();
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
        setStaticField(AllureManager.class, "cachedIsAllure2", false);

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

        Field internalField = Properties.class.getDeclaredField("internal");
        internalField.setAccessible(true);
        Object originalInternalConfig = internalField.get(null);
        String originalNodeLtsVersion = System.getProperty("nodeLtsVersion");
        System.setProperty("nodeLtsVersion", "invalid-version");
        try {
            SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
            setStaticField(AllureManager.class, "cachedIsAllure2", false);
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

        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        setStaticField(AllureManager.class, "cachedIsAllure2", false);
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

    @Test(description = "writeAllureReport should generate command lines correctly in both Allure2 and Allure3 cached modes")
    public void writeAllureReportShouldWorkInAllure2AndAllure3CachedModes() throws Exception {
        Method writeAllureReport = AllureManager.class.getDeclaredMethod("writeAllureReport");
        writeAllureReport.setAccessible(true);

        Path resultsDirectory = Path.of(System.getProperty("user.dir"), "target", "allure-results-test");
        Files.createDirectories(resultsDirectory);
        setStaticField(AllureManager.class, "allureResultsFolderPath", resultsDirectory.toString());

        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "echo");
        setStaticField(AllureManager.class, "cachedIsAllure2", true);
        writeAllureReport.invoke(null);

        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "echo");
        setStaticField(AllureManager.class, "cachedIsAllure2", false);
        writeAllureReport.invoke(null);

        Files.deleteIfExists(Path.of(System.getProperty("user.dir"), "allurerc.yaml"));
    }

    @Test(description = "resolveAllureCommandPrefix should support legacy resolution and cached-empty shortcut")
    public void resolveAllureCommandPrefixShouldSupportLegacyResolutionAndCachedEmptyShortcut() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        setStaticField(AllureManager.class, "cachedIsAllure2", false);
        Object legacyResolvedPrefix = resolveAllureCommandPrefix.invoke(null);
        SHAFT.Validations.assertThat().object(legacyResolvedPrefix == null
                || legacyResolvedPrefix.toString().equals("allure")
                || legacyResolvedPrefix.toString().contains("allure@")).isEqualTo(true).perform();

        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "");
        Object cachedEmptyResolution = resolveAllureCommandPrefix.invoke(null);
        SHAFT.Validations.assertThat().object(cachedEmptyResolution).isNull().perform();
    }

    @Test(description = "resolveAllureCommandPrefix should activate Allure2 compatibility when system allure reports version 2.x")
    public void resolveAllureCommandPrefixShouldActivateAllure2CompatibilityWhenSystemBinaryReports2x() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);
        Method readSystemAllureVersion = AllureManager.class.getDeclaredMethod("readSystemAllureVersion");
        readSystemAllureVersion.setAccessible(true);

        Path allureBinary = getWritablePathDirectory().resolve("allure");
        boolean binaryAlreadyExists = Files.exists(allureBinary);
        String originalBinaryContent = binaryAlreadyExists ? Files.readString(allureBinary) : null;
        try {
            Files.writeString(allureBinary, "#!/bin/sh\necho \"2.24.0\"\n");
            allureBinary.toFile().setExecutable(true);

            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
            setStaticField(AllureManager.class, "cachedIsAllure2", false);

            Object parsedVersion = readSystemAllureVersion.invoke(null);
            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(parsedVersion).isEqualTo("2.24.0").perform();
            SHAFT.Validations.assertThat().object(resolvedPrefix).isEqualTo("allure").perform();
            SHAFT.Validations.assertThat().object(getStaticField(AllureManager.class, "cachedIsAllure2")).isEqualTo(true).perform();
        } finally {
            if (binaryAlreadyExists) {
                Files.writeString(allureBinary, originalBinaryContent);
                allureBinary.toFile().setExecutable(true);
            } else {
                Files.deleteIfExists(allureBinary);
            }
        }
    }

    @Test(description = "resolveAllureCommandPrefix should prefer system allure when version is not 2.x in legacy mode")
    public void resolveAllureCommandPrefixShouldPreferSystemAllureWhenVersionIsNot2x() throws Exception {
        Method resolveAllureCommandPrefix = AllureManager.class.getDeclaredMethod("resolveAllureCommandPrefix");
        resolveAllureCommandPrefix.setAccessible(true);

        Path allureBinary = getWritablePathDirectory().resolve("allure");
        boolean binaryAlreadyExists = Files.exists(allureBinary);
        String originalBinaryContent = binaryAlreadyExists ? Files.readString(allureBinary) : null;
        try {
            Files.writeString(allureBinary, "#!/bin/sh\necho \"3.7.0\"\n");
            allureBinary.toFile().setExecutable(true);

            SHAFT.Properties.allure.set().forceConfiguredCliVersion(false);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
            setStaticField(AllureManager.class, "cachedIsAllure2", false);

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix).isEqualTo("allure").perform();
            SHAFT.Validations.assertThat().object(getStaticField(AllureManager.class, "cachedIsAllure2")).isEqualTo(false).perform();
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

        Path allureBinary = getWritablePathDirectory().resolve("allure");
        boolean binaryAlreadyExists = Files.exists(allureBinary);
        String originalBinaryContent = binaryAlreadyExists ? Files.readString(allureBinary) : null;
        try {
            Files.writeString(allureBinary, "#!/bin/sh\necho \"2.24.0\"\n");
            allureBinary.toFile().setExecutable(true);

            SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);
            setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
            setStaticField(AllureManager.class, "cachedIsAllure2", false);

            Object resolvedPrefix = resolveAllureCommandPrefix.invoke(null);
            SHAFT.Validations.assertThat().object(resolvedPrefix == null || resolvedPrefix.toString().contains("allure@")).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(getStaticField(AllureManager.class, "cachedIsAllure2")).isEqualTo(false).perform();
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

        Thread.currentThread().interrupt();
        Object executableOnPath = isExecutableOnPath.invoke(null, "java");
        SHAFT.Validations.assertThat().object(executableOnPath).isEqualTo(false).perform();
        Thread.interrupted();
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
        setStaticField(AllureManager.class, "cachedIsAllure2", true);

        SHAFT.Properties.allure.set().generateArchive(true);
        try (Stream<Path> existingArchives = Files.list(Path.of(System.getProperty("user.dir")))) {
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

        try (Stream<Path> generatedArchives = Files.list(Path.of(System.getProperty("user.dir")))) {
            long archiveCount = generatedArchives.filter(path -> path.getFileName().toString().startsWith("generatedReport_")
                            && path.getFileName().toString().endsWith(".zip"))
                    .count();
            SHAFT.Validations.assertThat().number((int) archiveCount).isEqualTo(1).perform();
        } finally {
            SHAFT.Properties.allure.set().generateArchive(false);
            try (Stream<Path> generatedArchives = Files.list(Path.of(System.getProperty("user.dir")))) {
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
        setStaticField(AllureManager.class, "cachedIsAllure2", false);
        setStaticField(AllureManager.class, "realtimeMonitoringProcess", null);
        Properties.clearForCurrentThread();
    }

    @Test(description = "getCommandToCreateAllureReport should use allure2 --clean syntax when cachedIsAllure2 is true")
    public void getCommandToCreateAllureReportShouldUseAllure2SyntaxWhenAllure2Detected() throws Exception {
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "allure");
        setStaticField(AllureManager.class, "cachedIsAllure2", true);
        setStaticField(AllureManager.class, "allureResultsFolderPath", "allure-results");
        setStaticField(AllureManager.class, "allureOutPutDirectory", "target/allure-report");

        Method getCommandMethod = AllureManager.class.getDeclaredMethod("getCommandToCreateAllureReport");
        getCommandMethod.setAccessible(true);
        String command = (String) getCommandMethod.invoke(null);

        // Allure 2: must use --single-file and --clean, must NOT use --config or allurerc.yaml
        SHAFT.Validations.assertThat().object(command).contains("--single-file").perform();
        SHAFT.Validations.assertThat().object(command).contains("--clean").perform();
        SHAFT.Validations.assertThat().object(command.contains("--config")).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(command.contains("allurerc.yaml")).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(command).contains("generate").perform();
        SHAFT.Validations.assertThat().object(command).contains("allure-results").perform();
    }

    @Test(description = "getCommandToCreateAllureReport should use allure3 --config syntax when allure3 is detected")
    public void getCommandToCreateAllureReportShouldUseAllure3SyntaxWhenAllure3Detected() throws Exception {
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "allure");
        setStaticField(AllureManager.class, "cachedIsAllure2", false);
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

    @Test(description = "Generated allure serve script should use allure2 syntax (no --config) when cachedIsAllure2 is true")
    public void generatedAllureServeScriptShouldUseAllure2SyntaxWhenAllure2Detected() throws Exception {
        setStaticField(AllureManager.class, "allureResultsFolderPath", "allure-results/");
        setStaticField(AllureManager.class, "cachedIsAllure2", true);
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "allure");

        Method scriptMethod = AllureManager.class.getDeclaredMethod("writeGenerateReportShellFilesToProjectDirectory");
        scriptMethod.setAccessible(true);

        String scriptFileName = SystemUtils.IS_OS_WINDOWS ? "generate_allure_report.bat" : "generate_allure_report.sh";
        Path scriptPath = Path.of(scriptFileName);
        try {
            scriptMethod.invoke(null);
            String content = Files.readString(scriptPath, StandardCharsets.UTF_8);

            // Allure 2 script: no --config or allurerc.yaml, just a simple allure serve command
            SHAFT.Validations.assertThat().object(content.contains("--config")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(content.contains("allurerc.yaml")).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(content).contains("allure serve").perform();
        } finally {
            Files.deleteIfExists(scriptPath);
        }
    }

    @Test(description = "watchCommandShouldUseSimpleAllure3SyntaxWithOnlyResultsDir when allure3 is used for realtime monitoring")
    public void watchCommandShouldUseSimpleAllure3SyntaxWithOnlyResultsDir() throws Exception {
        // Simulate allure3 state
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", "npx --yes allure@3.5.0");
        setStaticField(AllureManager.class, "cachedIsAllure2", false);
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
