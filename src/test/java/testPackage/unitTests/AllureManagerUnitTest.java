package testPackage.unitTests;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.tools.io.internal.AllureManager;
import org.testng.Assert;
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
        Assert.assertEquals(root.path("steps").size(), 2);

        JsonNode step1 = root.path("steps").get(0);
        Assert.assertEquals(step1.path("statusDetails").path("message").asText(), "");

        JsonNode step2 = root.path("steps").get(1);
        Assert.assertEquals(step2.path("statusDetails").path("trace").asText(), "x");
        Assert.assertEquals(step2.path("statusDetails").path("message").asText(), "");
        Assert.assertEquals(step2.path("steps").get(0).path("statusDetails").path("message").asText(), "");

        Assert.assertEquals(root.path("befores").size(), 1,
                "Empty fixtures should be pruned from befores/afters arrays");
        Assert.assertEquals(root.path("befores").get(0).path("steps").get(0)
                .path("statusDetails").path("message").asText(), "");
    }

    @Test(description = "getResultsPath should trim only a trailing separator")
    public void getResultsPathShouldTrimOnlyTrailingSeparator() throws Exception {
        Field pathField = AllureManager.class.getDeclaredField("allureResultsFolderPath");
        pathField.setAccessible(true);

        Method getResultsPath = AllureManager.class.getDeclaredMethod("getResultsPath");
        getResultsPath.setAccessible(true);

        pathField.set(null, "allure-results/");
        Assert.assertEquals(getResultsPath.invoke(null), "allure-results");

        pathField.set(null, "allure-results");
        Assert.assertEquals(getResultsPath.invoke(null), "allure-results");

        pathField.set(null, "");
        Assert.assertEquals(getResultsPath.invoke(null), "");
    }

    @Test(description = "AllureManager utility class constructor should be blocked")
    public void constructorShouldThrowIllegalStateException() throws Exception {
        Constructor<AllureManager> constructor = AllureManager.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException exception = Assert.expectThrows(InvocationTargetException.class, constructor::newInstance);
        Assert.assertTrue(exception.getCause() instanceof IllegalStateException);
        Assert.assertEquals(exception.getCause().getMessage(), "Utility class");
    }
}
