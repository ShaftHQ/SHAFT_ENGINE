package testPackage.locator;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * #5217: smartLoginFixture must expose case-distinct login accessible names
 * so testSmartLocators3/8 are not only proving LOGIN via toUpperCase().
 */
public class SmartLoginFixtureInventoryTest {
    private static final Path FIXTURE = Path.of("src/test/resources/testDataFiles/smartLoginFixture.html");

    @Test
    public void fixtureExposesCaseDistinctLoginAccessibleNames() throws IOException {
        Assert.assertTrue(Files.isRegularFile(FIXTURE), "missing " + FIXTURE.toAbsolutePath());
        String html = Files.readString(FIXTURE);

        Assert.assertTrue(html.contains("value=\"login\""),
                "fixture must expose accessible name login as its own control");
        Assert.assertTrue(html.contains("value=\"Login\""),
                "fixture must expose accessible name Login as its own control");
        Assert.assertTrue(html.contains("value=\"LOGIN\""),
                "fixture must keep LOGIN for the existing uppercase submit");
        Assert.assertTrue(html.contains(">Submit<"),
                "testSmartLocators2 still requires Username/Password/Submit");
    }
}
