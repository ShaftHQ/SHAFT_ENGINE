package testPackage.unitTests;

import com.shaft.api.RestActions;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import io.restassured.response.Response;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link com.shaft.validation.internal.RestValidationsBuilder}.
 *
 * <p>Exercises {@code extractedJsonValue}, {@code extractedJsonValueAsList},
 * {@code body}, and {@code time} builder methods using a live REST call to
 * JSONPlaceholder (a public free API).  All tests set {@code targetStatusCode=0}
 * so they skip status-code validation and focus only on the builder logic.
 */
public class RestValidationsBuilderUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    private Response fetchPost1() {
        return RestActions.buildNewRequest(
                        "https://jsonplaceholder.typicode.com/", "posts/1",
                        RestActions.RequestType.GET)
                .setTargetStatusCode(0)
                .performRequest()
                .getResponse();
    }

    private Response fetchPosts() {
        return RestActions.buildNewRequest(
                        "https://jsonplaceholder.typicode.com/", "posts",
                        RestActions.RequestType.GET)
                .setTargetStatusCode(0)
                .performRequest()
                .getResponse();
    }

    // ─── extractedJsonValue ───────────────────────────────────────────────────

    @Test(description = "extractedJsonValue isEqualTo: string field equals expected")
    public void extractedJsonValueIsEqualToShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.assertThat().response(response)
                    .extractedJsonValue("userId").isEqualTo("1").perform();
        }
    }

    @Test(description = "extractedJsonValue contains: string field contains partial text")
    public void extractedJsonValueContainsShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.assertThat().response(response)
                    .extractedJsonValue("title").contains("sunt").perform();
        }
    }

    @Test(description = "extractedJsonValue isNotNull: existing field should not be null")
    public void extractedJsonValueIsNotNullShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.assertThat().response(response)
                    .extractedJsonValue("body").isNotNull().perform();
        }
    }

    // ─── extractedJsonValueAsList ─────────────────────────────────────────────

    @Test(description = "extractedJsonValueAsList: root path returns non-empty list")
    public void extractedJsonValueAsListShouldPass() {
        Response response = fetchPosts();
        if (response != null) {
            Validations.assertThat().response(response)
                    .extractedJsonValueAsList("$").isNotNull().perform();
        }
    }

    // ─── body ─────────────────────────────────────────────────────────────────

    @Test(description = "body contains: response body contains expected text")
    public void bodyContainsShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.assertThat().response(response)
                    .body().contains("userId").perform();
        }
    }

    @Test(description = "body isNotNull: response body should not be null")
    public void bodyIsNotNullShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.assertThat().response(response)
                    .body().isNotNull().perform();
        }
    }

    // ─── time ─────────────────────────────────────────────────────────────────

    @Test(description = "time isGreaterThan 0: response time should be positive")
    public void timeIsGreaterThanZeroShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.assertThat().response(response)
                    .time().isGreaterThan(0).perform();
        }
    }

    @Test(description = "time isGreaterThanOrEquals 0: response time should be >= 0")
    public void timeIsGreaterThanOrEqualsZeroShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.assertThat().response(response)
                    .time().isGreaterThanOrEquals(0).perform();
        }
    }

    // ─── verifyThat soft assertions ───────────────────────────────────────────

    @Test(description = "verifyThat response body: soft assertion on body text")
    public void verifyThatResponseBodyShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.verifyThat().response(response).body().contains("userId").perform();
        }
    }

    @Test(description = "verifyThat extractedJsonValue: soft assertion on JSON field")
    public void verifyThatExtractedJsonValueShouldPass() {
        Response response = fetchPost1();
        if (response != null) {
            Validations.verifyThat().response(response)
                    .extractedJsonValue("userId").isEqualTo("1").perform();
        }
    }
}
