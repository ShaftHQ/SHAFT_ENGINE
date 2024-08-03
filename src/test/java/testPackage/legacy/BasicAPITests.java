package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class BasicAPITests {
    SHAFT.API api;

    @Test
    public void apiTest(){
        api = new SHAFT.API("https://fakerestapi.azurewebsites.net");
        api.get("/api/v1/Authors")
                .performRequest();
    }
}
