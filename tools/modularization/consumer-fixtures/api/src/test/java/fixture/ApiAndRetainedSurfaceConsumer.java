package fixture;

import com.shaft.driver.SHAFT;
import io.cucumber.java.en.Given;

public class ApiAndRetainedSurfaceConsumer {
    @Given("a retained SHAFT consumer surface")
    public void compilesRetainedSurface() {
        SHAFT.API api = new SHAFT.API("https://example.invalid");
        api.get("/health").setTargetStatusCode(200);
        SHAFT.CLI.file();
        SHAFT.Report.log("consumer fixture");
        SHAFT.DB database = null;
        if (database != null) {
            database.executeSelectQuery("SELECT 1");
        }
    }
}
