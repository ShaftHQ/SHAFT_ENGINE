package testPackage01.unitTests;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.RequestType;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Objects;

public class tests_restActions {

    @Test
    public void getPostsAndAssertBodyForSpecificTitle() {
        String serviceURI = "https://jsonplaceholder.typicode.com/";

//        Response posts = (new RestActions(serviceURI)).performRequest(RequestType.GET, 200, "posts");
        Response posts = RestActions.buildNewRequest(serviceURI, "posts", RequestType.GET).performRequest();

        List<Object> postsList = RestActions.getResponseJSONValueAsList(posts, "");
        Objects.requireNonNull(postsList).forEach(post -> {
            if (Objects.equals(RestActions.getResponseJSONValue(post, "title"), "qui est esse")) {
                Validations.assertThat().response(post).extractedJsonValue("body").contains("qui neque nisi nulla").perform();
            }
        });
    }
//
//    @Test
//    public void getAnimals() {
//        RestActions api = new RestActions("https://cat-fact.herokuapp.com");
//        Response response = api.performRequest(RequestType.GET, 200, "/facts/random?animal_type=cat&amount=1");
//        String actualResponse = RestActions.getResponseJSONValue(response, "text");
//        Assertions.assertEquals("", actualResponse, AssertionComparisonType.EQUALS, AssertionType.NEGATIVE);
//    }

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

}
