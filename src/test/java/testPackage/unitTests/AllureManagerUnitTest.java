package testPackage.unitTests;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.AllureManager;
import org.testng.annotations.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

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
}
