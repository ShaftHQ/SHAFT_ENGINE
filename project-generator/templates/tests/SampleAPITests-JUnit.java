package {{PACKAGE_NAME}};

import com.shaft.api.RequestBuilder;
import com.shaft.driver.SHAFT;
import io.restassured.http.ContentType;
import org.junit.jupiter.api.*;

public class SampleAPITests {
    private SHAFT.API api;
    private SHAFT.TestData.JSON testData;

    // Sample API endpoint
    private final String baseUrl = "https://jsonplaceholder.typicode.com";

    @BeforeAll
    public static void beforeAll() {
        // Optional: Setup actions that run once before all tests
    }

    @BeforeEach
    public void beforeEach() {
        api = new SHAFT.API(baseUrl);
        testData = new SHAFT.TestData.JSON("testData.json");
    }

    @Test
    @DisplayName("Get a single user and validate the response")
    public void testGetSingleUser() {
        api.get("/users/1")
                .perform();

        api.assertThat().response().isEqualTo(200)
                .and().assertThat().body().contains("Leanne Graham")
                .and().assertThat().jsonPath("$.id").isEqualTo(1);
    }

    @Test
    @DisplayName("Get all posts and validate the response count")
    public void testGetAllPosts() {
        api.get("/posts")
                .perform();

        api.assertThat().response().isEqualTo(200)
                .and().assertThat().jsonPath("$.size()").isEqualTo(100);
    }

    @Test
    @DisplayName("Create a new post via POST request")
    public void testCreatePost() {
        String requestBody = """
                {
                    "title": "SHAFT Engine Test",
                    "body": "This is a test post created using SHAFT Engine",
                    "userId": 1
                }
                """;

        api.post("/posts")
                .setRequestBody(requestBody)
                .setContentType(ContentType.JSON)
                .perform();

        api.assertThat().response().isEqualTo(201)
                .and().assertThat().jsonPath("$.title").isEqualTo("SHAFT Engine Test");
    }

    @AfterEach
    public void afterEach() {
        // Optional: Cleanup after each test
    }

    @AfterAll
    public static void afterAll() {
        // Optional: Cleanup actions that run once after all tests
    }
}
