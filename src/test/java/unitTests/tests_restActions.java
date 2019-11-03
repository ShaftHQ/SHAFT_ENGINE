package unitTests;

import java.util.List;

import org.json.simple.JSONObject;
import org.testng.annotations.Test;

import com.shaft.api.RestActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Verifications;
import com.shaft.validation.Verifications.VerificationComparisonType;
import com.shaft.validation.Verifications.VerificationType;

import io.restassured.http.ContentType;
import io.restassured.response.Response;

public class tests_restActions {

    @Test
    public void mockOK() {
	String serviceURI = "https://testttal.free.beeceptor.com/";
	Response mockOK = (new RestActions(serviceURI)).performRequest("GET", "200", "mockOK", null, null, null,
		ContentType.ANY);
	Assertions.assertEquals("Awesome!", RestActions.getResponseJSONValue(mockOK, "status"),
		AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
    }

    @Test
    public void mockUnauthorizedAccess() {
	String serviceURI = "https://testttal.free.beeceptor.com/";
	Response mockUnauthorizedAccess = (new RestActions(serviceURI)).performRequest("DELETE", "401",
		"mockUnauthorized", null, null, null, ContentType.ANY);
	Assertions.assertEquals("Unauthorized Access",
		RestActions.getResponseJSONValue(mockUnauthorizedAccess, "status"), AssertionComparisonType.EQUALS,
		AssertionType.POSITIVE);
    }

    @Test
    public void mockNotModified() {
	String serviceURI = "https://testttal.free.beeceptor.com/";
	Response mockNotModified = (new RestActions(serviceURI)).performRequest("PATCH", "304", "mockNotModified", null,
		null, null, ContentType.ANY);
	Assertions.assertEquals(304, RestActions.getResponseStatusCode(mockNotModified), AssertionComparisonType.EQUALS,
		AssertionType.POSITIVE);
    }

    @Test
    public void getPostsAndAssertBodyForSpecificTitle() {
	String serviceURI = "https://jsonplaceholder.typicode.com/";

	Response posts = (new RestActions(serviceURI)).performRequest("GET", "200", "posts", null, null, null,
		ContentType.ANY);
	List<Object> postsList = RestActions.getResponseJSONValueAsList(posts, "");
	postsList.forEach(post -> {
	    if (RestActions.getResponseJSONValue(post, "title").equals("qui est esse")) {
		Assertions.assertEquals("qui neque nisi nulla", RestActions.getResponseJSONValue(post, "body"),
			AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
	    }
	});
    }

    @SuppressWarnings("unchecked")
    @Test
    public void createPostAndAssertContent() {
	String serviceURI = "https://jsonplaceholder.typicode.com/";

	String newPostTitle = "Test Title";
	String newPostBody = "Test Body";
	String userId = "1";

	JSONObject object = new JSONObject();
	object.put("title", newPostTitle);
	object.put("body", newPostBody);
	object.put("userId", userId);

	Response newPost = (new RestActions(serviceURI)).performRequest("POST", "201", "posts", null, null, object,
		ContentType.JSON);
	Verifications.verifyEquals(newPostTitle, RestActions.getResponseJSONValue(newPost, "title"),
		VerificationComparisonType.EQUALS, VerificationType.POSITIVE);
	Verifications.verifyEquals(newPostBody, RestActions.getResponseJSONValue(newPost, "body"),
		VerificationComparisonType.EQUALS, VerificationType.POSITIVE);
	Verifications.verifyEquals(userId, RestActions.getResponseJSONValue(newPost, "userId"),
		VerificationComparisonType.EQUALS, VerificationType.POSITIVE);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void editPostAndAssertNewTitle() {
	String serviceURI = "https://jsonplaceholder.typicode.com/";

	String newPostTitle = "Test Title";

	JSONObject object = new JSONObject();
	object.put("title", newPostTitle);

	Response posts = (new RestActions(serviceURI)).performRequest("PATCH", "200", "posts/1", null, null, object,
		ContentType.JSON);
	Assertions.assertEquals(newPostTitle, RestActions.getResponseJSONValue(posts, "title"),
		AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
    }
}
