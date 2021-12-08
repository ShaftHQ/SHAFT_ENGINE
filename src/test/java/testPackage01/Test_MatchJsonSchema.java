package testPackage01;

import com.shaft.api.RestActions;
import com.shaft.driver.DriverFactory;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.testng.annotations.Test;


public class Test_MatchJsonSchema {

    @Test
    public void checkAPI_ResponseSchema() {

        Response res = DriverFactory.getAPIDriver("http://api.zippopotam.us/")
                .buildNewRequest("us/90210", RestActions.RequestType.GET)
                .performRequest();


        Validations.assertThat()
                .response(res)
                .matchesSchema(System.getProperty("testDataFolderPath") + "schema.json")
                .perform();

    }

}
