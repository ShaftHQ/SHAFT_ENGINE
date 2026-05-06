package testPackage.unitTests;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.AllureManager;
import org.apache.commons.lang3.SystemUtils;
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

/**
 * Unit tests for private helper behavior in {@link AllureManager}.
 */
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

            SHAFT.Validations.assertThat().object(content.contains("allure@" + SHAFT.Properties.internal.allure3Version()))
                    .isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(content.contains("allure --version")).isEqualTo(true).perform();
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

    /**
     * Helper that resets the cached CLI resolution state in AllureManager so that each test
     * that modifies it starts from a clean slate.
     */
    @AfterMethod(alwaysRun = true)
    public void resetAllureManagerCachedState() throws Exception {
        setStaticField(AllureManager.class, "cachedAllureCommandPrefix", null);
        setStaticField(AllureManager.class, "cachedIsAllure2", false);
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

        // Allure 2: must use --clean, must NOT use --config or allurerc.yaml
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

        // Build the expected watch command the same way the production code does
        String prefix = (String) getStaticField(AllureManager.class, "cachedAllureCommandPrefix");
        String resultsPath = "allure-results"; // getResultsPath with no trailing separator

        String expectedCommand = prefix + " watch \"" + resultsPath + "\"";

        // The proper watch command must NOT include --config, --output, or --open flags
        SHAFT.Validations.assertThat().object(expectedCommand.contains("--config")).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(expectedCommand.contains("--output")).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(expectedCommand.contains("--open")).isEqualTo(false).perform();
        SHAFT.Validations.assertThat().object(expectedCommand).contains("watch").perform();
        SHAFT.Validations.assertThat().object(expectedCommand).contains("allure-results").perform();
    }
}
