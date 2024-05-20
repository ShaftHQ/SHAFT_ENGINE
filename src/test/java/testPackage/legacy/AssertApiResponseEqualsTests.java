package testPackage.legacy;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.RequestType;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.testng.annotations.Test;

public class AssertApiResponseEqualsTests {
    @Test
    public void assertApiResponseEquals_expectedToPass() {
        RestActions apiObject = new RestActions("https://jsonplaceholder.typicode.com");
        Response users = apiObject.buildNewRequest("/users", RequestType.GET).setTargetStatusCode(200).performRequest();
        RestActions.getResponseJSONValueAsList(users, "$").forEach(user -> {
            if (RestActions.getResponseJSONValue(user, "name").equals("Leanne Graham")) {
                Validations.verifyThat().response(user).extractedJsonValue("email").isEqualTo("Sincere@april.biz").perform();
                Validations.assertThat().response(user).extractedJsonValue("username").isEqualTo("Bret").perform();
            }
        });
    }
}
