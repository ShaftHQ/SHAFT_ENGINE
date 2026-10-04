package testPackage;

import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

public class TestClass {
    static SHAFT.TestData.JSON testData;
    SHAFT.API driver;
    String serviceURI = "https://geocoding-api.open-meteo.com/v1/";
    String forecastServiceURI = "https://api.open-meteo.com/v1/";

    @BeforeAll
    static void beforeAll() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @Test
    void getCountryInfoUsingCapitalName() {
        driver.get("search")
                .setUrlArguments("name=Cairo&count=1")
                .perform()
                .assertThatResponse().extractedJsonValue("results[0].country").isEqualTo(testData.get("expectedCountryName"));
    }

    @Test
    void searchCityUsingQueryParameters() {
        driver.get("search")
                .setParameters(Map.of("name", testData.get("cityName"), "count", 1, "language", "en"), RestActions.ParametersType.QUERY)
                .perform();

        driver.assertThatResponse().statusCodeValue().isEqualTo(200);
        driver.assertThatResponse().headerValue("Content-Type").contains("application/json");
        driver.assertThatResponse().jsonValue("results[0].country_code").isEqualTo(testData.get("expectedCountryCode"));
        driver.assertThatResponse().responseTimeMillis().isLessThan(10000);
    }

    @Test
    void verifyCityDetailsUsingSoftAssertions() {
        driver.get("search")
                .setUrlArguments("name=Cairo&count=1")
                .perform();

        // verifyThat collects every failure and reports them together at the end of the test
        driver.verifyThatResponse().jsonValue("results[0].name").isEqualTo(testData.get("cityName"));
        driver.verifyThatResponse().jsonValue("results[0].timezone").isEqualTo(testData.get("expectedTimezone"));
        long population = Long.parseLong(driver.getResponseJSONValue("results[0].population"));
        SHAFT.Validations.verifyThat().number(population).isGreaterThan(1000000);
    }

    @Test
    void getCityByIdMatchesJsonSchema() {
        driver.get("get")
                .setUrlArguments("id=" + testData.get("cityId"))
                .perform();

        driver.assertThatResponse().matchesSchema("geocodingLocationSchema.json");
        driver.assertThatResponse().jsonValue("name").isEqualTo(testData.get("cityName"));
    }

    @Test
    void chainRequestsToGetWeatherForecastOfCity() {
        driver.get("search")
                .setUrlArguments("name=Cairo&count=1")
                .perform();
        String latitude = driver.getResponseJSONValue("results[0].latitude");
        String longitude = driver.getResponseJSONValue("results[0].longitude");

        SHAFT.API forecast = new SHAFT.API(forecastServiceURI);
        forecast.get("forecast")
                .setUrlArguments("latitude=" + latitude + "&longitude=" + longitude
                        + "&daily=temperature_2m_max,temperature_2m_min&timezone=auto&forecast_days=3")
                .perform();

        forecast.assertThatResponse().jsonValue("timezone").isEqualTo(testData.get("expectedTimezone"));
        SHAFT.Validations.assertThat().number(forecast.getResponseJSONValueAsList("daily.time").size()).isEqualTo(3);
    }

    @Test
    void invalidLatitudeReturnsBadRequest() {
        SHAFT.API forecast = new SHAFT.API(forecastServiceURI);
        forecast.get("forecast")
                .setUrlArguments("latitude=999&longitude=31.25&current=temperature_2m")
                .setTargetStatusCode(400)
                .perform();

        forecast.assertThatResponse().jsonValue("error").isEqualTo("true");
        forecast.assertThatResponse().jsonValue("reason").contains("Latitude must be in range");
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
