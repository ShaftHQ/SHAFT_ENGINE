package testPackage;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class TestClass {
    static SHAFT.TestData.JSON testData;
    SHAFT.API driver;
    String serviceURI = "https://geocoding-api.open-meteo.com/v1/";

    @BeforeAll
    static void beforeAll() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @Test
    void getCountryInfoUsingCapitalName() {
        driver.get("search")
                .setUrlArguments("name=Cairo&count=1")
                .perform()
                .assertThatResponse().extractedJsonValue("results[0].country").isEqualTo(testData.get("expectedCountryName")).perform();
    }

    @BeforeEach
    void beforeEach() {
        driver = new SHAFT.API(serviceURI);
    }

    @AfterEach
    void afterEach() {
        driver = null;
    }
}
