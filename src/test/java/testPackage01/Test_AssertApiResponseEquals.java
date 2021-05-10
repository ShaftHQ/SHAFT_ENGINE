package testPackage01;

import org.testng.annotations.Test;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.RequestType;
import com.shaft.validation.Assertions;
import com.shaft.validation.Verifications;

import io.restassured.response.Response;

public class Test_AssertApiResponseEquals {
    @Test
    public void assertApiResponseEquals_expectedToPass() {
        RestActions apiObject = new RestActions("https://jsonplaceholder.typicode.com");
        Response users = apiObject.buildNewRequest("/users", RequestType.GET).setTargetStatusCode(200).performRequest();

        RestActions.getResponseJSONValueAsList(users, "$").forEach(user -> {
            if (RestActions.getResponseJSONValue(user, "name").equals("Leanne Graham")) {
            	
               Verifications.verifyApiResponseEquals(user, "Sincere@april.biz", "email");
               Assertions.assertApiResponseEquals(user, "Bret", "username");
            }
        });
    }
}
