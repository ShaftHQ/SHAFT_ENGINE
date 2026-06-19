package junitTestPackage;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import testPackage.unitTests.LocalApiServer;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

class JunitApiSmokeTest {
    private static LocalApiServer localApiServer;
    private static String localApiBaseUrl;

    @BeforeAll
    static void startLocalApiServer() {
        localApiServer = LocalApiServer.start();
        localApiBaseUrl = localApiServer.baseUrl();
    }

    @AfterAll
    static void stopLocalApiServer() {
        if (localApiServer != null) {
            localApiServer.close();
        }
    }

    @Test
    void shaftApiShouldReadJsonFromLocalFixtureServer() {
        assertDoesNotThrow(() -> {
            SHAFT.API api = new SHAFT.API(localApiBaseUrl);

            api.get("/posts/1").setTargetStatusCode(200).perform();

            api.assertThatResponse().extractedJsonValue("id").isEqualTo("1").perform();
            api.assertThatResponse().body().contains("sunt aut facere").perform();
        });
    }
}
