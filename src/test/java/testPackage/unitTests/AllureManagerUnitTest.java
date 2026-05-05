package testPackage.unitTests;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.AllureManager;
import org.apache.commons.lang3.SystemUtils;
import org.testng.annotations.Test;

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
        scriptMethod.invoke(null);

        String scriptFileName = SystemUtils.IS_OS_WINDOWS ? "generate_allure_report.bat" : "generate_allure_report.sh";
        Path scriptPath = Path.of(scriptFileName);
        try {
            String content = Files.readString(scriptPath, StandardCharsets.UTF_8);

            SHAFT.Validations.assertThat().object(content.contains("allure@" + SHAFT.Properties.internal.allure3Version()))
                    .isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(content.contains("allure --version")).isEqualTo(true).perform();
        } finally {
            Files.deleteIfExists(scriptPath);
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
}
