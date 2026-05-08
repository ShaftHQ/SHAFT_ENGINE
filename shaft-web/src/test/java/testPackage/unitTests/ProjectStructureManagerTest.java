package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ProjectStructureManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

/**
 * Unit tests for {@link ProjectStructureManager} service-file generation behavior.
 */
public class ProjectStructureManagerTest {
    private Path tempServicesDirPath;
    private String savedExecutionAddress;
    private String savedServicesPath;

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws IOException {
        if (savedExecutionAddress != null) {
            SHAFT.Properties.platform.set().executionAddress(savedExecutionAddress);
        }
        if (savedServicesPath != null) {
            SHAFT.Properties.paths.set().services(savedServicesPath);
        }
        deleteTempServicesDirectory();
    }

    @Test(description = "ProjectStructureManager generates TestNG listener and transformer META-INF service files together")
    public void generatesTestNgListenerAndAnnotationTransformerServiceFiles() throws IOException {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        savedServicesPath = SHAFT.Properties.paths.services();

        tempServicesDirPath = Files.createTempDirectory("shaft-services");
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.paths.set().services(tempServicesDirPath.toString() + File.separator);

        ProjectStructureManager.initialize(ProjectStructureManager.RunType.TESTNG);

        Path testNgListenerFile = tempServicesDirPath.resolve("org.testng.ITestNGListener");
        Path annotationTransformerFile = tempServicesDirPath.resolve("org.testng.IAnnotationTransformer");

        SHAFT.Validations.assertThat().object(Files.exists(testNgListenerFile)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.exists(annotationTransformerFile)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.readString(testNgListenerFile).trim())
                .isEqualTo("com.shaft.listeners.TestNGListener").perform();
        SHAFT.Validations.assertThat().object(Files.readString(annotationTransformerFile).trim())
                .isEqualTo("com.shaft.listeners.TestNGListener").perform();
    }

    private void deleteTempServicesDirectory() throws IOException {
        if (tempServicesDirPath == null || !Files.exists(tempServicesDirPath)) {
            return;
        }
        try (var paths = Files.walk(tempServicesDirPath)) {
            paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
        }
    }
}
