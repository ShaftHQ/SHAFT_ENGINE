package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ProjectStructureManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;

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

    @Test(description = "identifyRunType(suites) detects a Cucumber TestNG runner class in the suite even when " +
            "called before any Cucumber Runner stack frame exists, i.e. from IAlterSuiteListener#alter")
    public void identifyRunTypeWithSuitesDetectsCucumberTestNgRunnerClass() {
        XmlSuite suite = new XmlSuite();
        XmlTest xmlTest = new XmlTest(suite);
        xmlTest.setXmlClasses(List.of(new XmlClass(cucumberTestRunner.CucumberTests.class)));

        ProjectStructureManager.RunType runType = ProjectStructureManager.identifyRunType(List.of(suite));

        SHAFT.Validations.assertThat().object(runType).isEqualTo(ProjectStructureManager.RunType.CUCUMBER).perform();
    }

    @Test(description = "identifyRunType(suites) still resolves TESTNG for a suite that hosts no Cucumber TestNG " +
            "runner class, proving the fix does not misdetect a plain TestNG suite as Cucumber")
    public void identifyRunTypeWithSuitesResolvesTestNgForPlainTestNgClass() {
        XmlSuite suite = new XmlSuite();
        XmlTest xmlTest = new XmlTest(suite);
        xmlTest.setXmlClasses(List.of(new XmlClass(ProjectStructureManagerTest.class)));

        ProjectStructureManager.RunType runType = ProjectStructureManager.identifyRunType(List.of(suite));

        SHAFT.Validations.assertThat().object(runType).isEqualTo(ProjectStructureManager.RunType.TESTNG).perform();
    }

    private void deleteTempServicesDirectory() throws IOException {
        if (tempServicesDirPath == null || !Files.exists(tempServicesDirPath)) {
            return;
        }
        deleteDirectory(tempServicesDirPath);
    }

    private void deleteDirectory(Path directory) throws IOException {
        try (var paths = Files.walk(directory)) {
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
