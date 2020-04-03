package unitTests;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.RequestType;
import com.shaft.tools.support.JavaActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Verifications;
import com.shaft.validation.Verifications.VerificationComparisonType;
import com.shaft.validation.Verifications.VerificationType;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import org.json.simple.JSONObject;
import org.testng.annotations.Test;

import java.util.List;

public class tests_restActions {

    @Test
    public void mockOK() {
        String serviceURI = "https://testttal.free.beeceptor.com/";
        Response mockOK = (new RestActions(serviceURI)).performRequest(RequestType.GET, 200, "mockOK");
        Assertions.assertEquals("Awesome!", RestActions.getResponseJSONValue(mockOK, "status"),
                AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
    }

    @Test
    public void mockUnauthorizedAccess() {
        String serviceURI = "https://testttal.free.beeceptor.com/";
        Response mockUnauthorizedAccess = (new RestActions(serviceURI)).performRequest(RequestType.DELETE, 401,
                "mockUnauthorized");
        Assertions.assertEquals("Unauthorized Access",
                RestActions.getResponseJSONValue(mockUnauthorizedAccess, "status"), AssertionComparisonType.EQUALS,
                AssertionType.POSITIVE);
    }

    @Test
    public void mockNotModified() {
        String serviceURI = "https://testttal.free.beeceptor.com/";
        Response mockNotModified = (new RestActions(serviceURI)).performRequest(RequestType.PATCH, 304,
                "mockNotModified");
        Assertions.assertEquals(304, RestActions.getResponseStatusCode(mockNotModified), AssertionComparisonType.EQUALS,
                AssertionType.POSITIVE);
    }

    @Test
    public void getPostsAndAssertBodyForSpecificTitle() {
        String serviceURI = "https://jsonplaceholder.typicode.com/";

        Response posts = (new RestActions(serviceURI)).performRequest(RequestType.GET, 200, "posts");
        List<Object> postsList = RestActions.getResponseJSONValueAsList(posts, "");
        postsList.forEach(post -> {
            if (RestActions.getResponseJSONValue(post, "title").equals("qui est esse")) {
                Assertions.assertEquals("qui neque nisi nulla", RestActions.getResponseJSONValue(post, "body"),
                        AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
            }
        });
    }

    @Test
    public void getAnimals() {
        RestActions api = new RestActions("https://cat-fact.herokuapp.com");
        Response response = api.performRequest(RequestType.GET, 200, "/facts/random?animal_type=cat&amount=1");
        String actualResponse = RestActions.getResponseJSONValue(response, "text");
        Assertions.assertEquals("", actualResponse, AssertionComparisonType.EQUALS, AssertionType.NEGATIVE);
    }
}
