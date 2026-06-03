package testPackage.unitTests;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.RequestType;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Objects;

public class RestActionsTests {
    private LocalApiServer localApiServer;
    private String localApiBaseUrl;

    @BeforeClass
    public void startLocalApiServer() {
        localApiServer = LocalApiServer.start();
        localApiBaseUrl = localApiServer.baseUrl();
    }

    @AfterClass(alwaysRun = true)
    public void stopLocalApiServer() {
        if (localApiServer != null) {
            localApiServer.close();
        }
    }

    @Test
    public void getPostsAndAssertBodyForSpecificTitle() {
        String serviceURI = localApiBaseUrl + "/";

//        Response posts = (new RestActions(serviceURI)).performRequest(RequestType.GET, 200, "posts");
        Response posts = RestActions.buildNewRequest(serviceURI, "posts", RequestType.GET).performRequest().getResponse();

        List<Object> postsList = RestActions.getResponseJSONValueAsList(posts, "$");
        Objects.requireNonNull(postsList).forEach(post -> {
            if (Objects.equals(RestActions.getResponseJSONValue(post, "title"), "qui est esse")) {
                Validations.assertThat().response(post).extractedJsonValue("body").contains("qui neque nisi nulla").perform();
            }
        });
    }

    @Test
    public void validateUserEmail() {
        RestActions apiObject = new RestActions(localApiBaseUrl);
        Response users = apiObject.buildNewRequest("/users", RequestType.GET).setTargetStatusCode(200).performRequest().getResponse();
        Validations.assertThat().object(RestActions.getResponseBody(users)).contains("Leanne Graham").perform();

        Objects.requireNonNull(RestActions.getResponseJSONValueAsList(users, "$")).forEach(user -> {
            if (Objects.equals(RestActions.getResponseJSONValue(user, "name"), "Leanne Graham")) {
                Validations.assertThat().response(user).extractedJsonValue("email").equals("Sincere@april.biz");
            }

        });
    }

    @Test
    public void validateUserId() {
        RestActions apiObject = new RestActions(localApiBaseUrl);
        Response users = apiObject.buildNewRequest("/users", RequestType.GET).setTargetStatusCode(200).performRequest().getResponse();

        String uerId = RestActions.getResponseJSONValueFromList(users, "$", "id", "name", "Chelsey Dietrich");
        Validations.assertThat().object(uerId).isEqualTo("5").perform();
    }

    @Test
    public void validateTodoId() {
        RestActions apiObject = new RestActions(localApiBaseUrl);
        Response users = apiObject.buildNewRequest("/todos", RequestType.GET).setTargetStatusCode(200).performRequest().getResponse();

        String todoId = RestActions.getResponseJSONValueFromList(users, "$", "id", "title", "delectus aut autem");
        Validations.assertThat().object(todoId).isEqualTo("1").perform();
    }

}
