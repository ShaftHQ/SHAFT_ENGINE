package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class TestClass {
    SHAFT.API driver;
    SHAFT.TestData.JSON testData;
    String serviceURI = "https://api.restcountries.com/countries/v5/";

    @Test
    public void getCountryInfoUsingCapitalName() {
        driver.get("capitals/{capital}".replace("{capital}", "Cairo"))
                .addHeader("Authorization", "Bearer " + RestCountriesCredentials.apiKey())
                .perform()
                .assertThatResponse().extractedJsonValue("data.objects[0].names.common").isEqualTo(testData.get("expectedCountryName")).perform();
    }

    @BeforeClass
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.API(serviceURI);
    }

    @AfterMethod
    public void afterMethod() {
        driver = null;
    }
}
