package testPackage;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class TestClass {
    static SHAFT.TestData.JSON testData;
    SHAFT.API driver;
    String serviceURI = "https://restcountries.com/v3.1/";

    @BeforeAll
    static void beforeAll() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @Test
    void getCountryInfoUsingCapitalName() {
        driver.get("capital/{capital}".replace("{capital}", "Cairo"))
                .perform()
                .assertThatResponse().extractedJsonValue("[0].name.common").isEqualTo(testData.get("expectedCountryName")).perform();
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
