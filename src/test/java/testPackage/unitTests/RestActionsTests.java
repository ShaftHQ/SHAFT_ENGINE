package testPackage.unitTests;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.RequestType;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Objects;

public class RestActionsTests {

    @Test
    public void getPostsAndAssertBodyForSpecificTitle() {
        String serviceURI = "https://jsonplaceholder.typicode.com/";

//        Response posts = (new RestActions(serviceURI)).performRequest(RequestType.GET, 200, "posts");
        Response posts = RestActions.buildNewRequest(serviceURI, "posts", RequestType.GET).performRequest();

        List<Object> postsList = RestActions.getResponseJSONValueAsList(posts, "$");
        Objects.requireNonNull(postsList).forEach(post -> {
            if (Objects.equals(RestActions.getResponseJSONValue(post, "title"), "qui est esse")) {
                Validations.assertThat().response(post).extractedJsonValue("body").contains("qui neque nisi nulla").perform();
            }
        });
    }

    @Test
    public void validateUserEmail() {
        RestActions apiObject = new RestActions("https://jsonplaceholder.typicode.com");
        Response users = apiObject.buildNewRequest("/users", RequestType.GET).setTargetStatusCode(200).performRequest();
        Validations.assertThat().object(RestActions.getResponseBody(users)).contains("Leanne Graham").perform();

        Objects.requireNonNull(RestActions.getResponseJSONValueAsList(users, "$")).forEach(user -> {
            if (Objects.equals(RestActions.getResponseJSONValue(user, "name"), "Leanne Graham")) {
                Validations.assertThat().response(user).extractedJsonValue("email").equals("Sincere@april.biz");
            }

        });
    }

    @Test
    public void validateUserId() {
        RestActions apiObject = new RestActions("https://jsonplaceholder.typicode.com");
        Response users = apiObject.buildNewRequest("/users", RequestType.GET).setTargetStatusCode(200).performRequest();

        String uerId = RestActions.getResponseJSONValueFromList(users, "$", "id", "name", "Chelsey Dietrich");
        Validations.assertThat().object(uerId).isEqualTo("5").perform();
    }

    @Test
    public void validateProductId() {
        RestActions apiObject = new RestActions("https://automationexercise.com/api");
        Response users = apiObject.buildNewRequest("/productsList", RequestType.GET).setTargetStatusCode(200).performRequest();

        String productId = RestActions.getResponseJSONValueFromList(users, "$.products", "id", "name", "Men Tshirt");
        Validations.assertThat().object(productId).isEqualTo("2").perform();
    }

}
