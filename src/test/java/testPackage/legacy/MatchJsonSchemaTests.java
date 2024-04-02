package testPackage.legacy;

import com.shaft.api.RestActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.testng.annotations.Test;


public class MatchJsonSchemaTests {

    @Test
    public void checkAPI_ResponseSchema() {

        Response res = DriverFactory.getAPIDriver("http://api.zippopotam.us/")
                .buildNewRequest("us/90210", RestActions.RequestType.GET)
                .performRequest();


        Validations.assertThat()
                .response(res)
                .matchesSchema(SHAFT.Properties.paths.testData() + "schema.json")
                .perform();

    }

}
