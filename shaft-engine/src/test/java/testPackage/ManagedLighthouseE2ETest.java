package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Collectors;

/** Opt-in real local-Chrome acceptance for the managed Lighthouse runtime. */
public class ManagedLighthouseE2ETest {
    @Test
    public void managedLighthouseProducesANonemptyHtmlReport() throws Exception {
        if (!Boolean.getBoolean("runManagedLighthouseE2E")) {
            throw new SkipException("Set -DrunManagedLighthouseE2E=true to run managed Lighthouse acceptance.");
        }
        Path reportDirectory = Path.of("lighthouse-reports").toAbsolutePath().normalize();
        Set<Path> before = reports(reportDirectory);
        SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
        try {
            driver.browser().navigateToURL("https://example.com/");
            driver.browser().generateLightHouseReport();
        } finally {
            driver.quit();
        }
        Set<Path> created = reports(reportDirectory);
        created.removeAll(before);
        Assert.assertEquals(created.size(), 1, "Managed Lighthouse should create exactly one HTML report.");
        String html = Files.readString(created.iterator().next()).toLowerCase(java.util.Locale.ROOT);
        Assert.assertTrue(html.contains("<html"), "Managed Lighthouse output should be an HTML document.");
    }

    private static Set<Path> reports(Path directory) throws Exception {
        if (!Files.isDirectory(directory)) return new java.util.HashSet<>();
        try (var reports = Files.list(directory)) {
            return reports.filter(path -> path.toString().endsWith(".html"))
                    .collect(Collectors.toCollection(java.util.HashSet::new));
        }
    }
}
