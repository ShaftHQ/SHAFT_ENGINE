package testPackage.unitTests;

import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.testng.Reporter;
import org.testng.annotations.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * Unit tests for failure logging behavior in {@link RestActions}.
 */
public class RestActionsFailureLoggingUnitTest {

    @Test(description = "failAction should emit one failure log entry for status-code failures")
    public void failActionShouldLogStatusCodeFailureOnce() throws Exception {
        Method failAction = RestActions.class.getDeclaredMethod("failAction",
                String.class, String.class, Object.class, RequestSpecification.class, Response.class, Throwable[].class);
        failAction.setAccessible(true);

        int previousOutputSize = Reporter.getOutput().size();
        InvocationTargetException exception = null;
        try {
            failAction.invoke(null,
                    "evaluateResponseStatusCode",
                    "Actual response status code \"404\" is a failure (Not between 200 and 299).",
                    null, null, null, new Throwable[0]);
        } catch (InvocationTargetException e) {
            exception = e;
        }

        List<String> logOutput = Reporter.getOutput();
        List<String> newEntries = logOutput.subList(previousOutputSize, logOutput.size());
        long matchingEntries = newEntries.stream()
                .filter(line -> line.contains("Evaluate response status code failed; Actual response status code \"404\" is a failure"))
                .count();

        SHAFT.Validations.assertThat().object(exception != null).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(exception.getCause().getClass().getName())
                .isEqualTo(RuntimeException.class.getName()).perform();
        SHAFT.Validations.assertThat().number(matchingEntries).isEqualTo(1).perform();
    }

    @Test(description = "failAction should include root cause in the single failure log entry")
    public void failActionShouldIncludeRootCauseWithoutDuplicateEntries() throws Exception {
        Method failAction = RestActions.class.getDeclaredMethod("failAction",
                String.class, String.class, Object.class, RequestSpecification.class, Response.class, Throwable[].class);
        failAction.setAccessible(true);

        RuntimeException rootCause = new RuntimeException("status code validation failed");
        int previousOutputSize = Reporter.getOutput().size();
        InvocationTargetException exception = null;
        try {
            failAction.invoke(null,
                    "handleException",
                    "https://restful-booker.herokuapp.com/booking/1, Response Time: 352ms.",
                    null, null, null, new Throwable[]{rootCause});
        } catch (InvocationTargetException e) {
            exception = e;
        }

        List<String> logOutput = Reporter.getOutput();
        List<String> newEntries = logOutput.subList(previousOutputSize, logOutput.size());
        long matchingEntries = newEntries.stream()
                .filter(line -> line.contains("Handle exception failed; https://restful-booker.herokuapp.com/booking/1, Response Time: 352ms."))
                .count();

        SHAFT.Validations.assertThat().object(exception != null).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(exception.getCause().getClass().getName())
                .isEqualTo(RuntimeException.class.getName()).perform();
        SHAFT.Validations.assertThat().object(exception.getCause().getMessage().contains("Root cause")).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().number(matchingEntries).isEqualTo(1).perform();
    }
}
