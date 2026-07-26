package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ProjectStructureManager;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlInclude;
import org.testng.xml.XmlPackage;
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

    @Test(description = "identifyRunType(suites) detects a Cucumber TestNG runner class declared via <packages> " +
            "(package scan) rather than an explicit <classes><class> entry")
    public void identifyRunTypeWithSuitesDetectsCucumberTestNgRunnerClassDeclaredViaPackages() {
        XmlSuite suite = new XmlSuite();
        XmlTest xmlTest = new XmlTest(suite);
        xmlTest.setXmlPackages(List.of(new XmlPackage("cucumberTestRunner")));

        ProjectStructureManager.RunType runType = ProjectStructureManager.identifyRunType(List.of(suite));

        SHAFT.Validations.assertThat().object(runType).isEqualTo(ProjectStructureManager.RunType.CUCUMBER).perform();
    }

    @Test(description = "identifyRunType(suites) detects a Cucumber TestNG runner class declared only inside a " +
            "child suite pulled in via <suite-file>, which the parent's own getTests() does not expose")
    public void identifyRunTypeWithSuitesDetectsCucumberTestNgRunnerClassInChildSuiteFile() {
        XmlSuite parentSuite = new XmlSuite();
        XmlSuite childSuite = new XmlSuite();
        XmlTest childXmlTest = new XmlTest(childSuite);
        childXmlTest.setXmlClasses(List.of(new XmlClass(cucumberTestRunner.CucumberTests.class)));
        parentSuite.getChildSuites().add(childSuite);

        ProjectStructureManager.RunType runType = ProjectStructureManager.identifyRunType(List.of(parentSuite));

        SHAFT.Validations.assertThat().object(runType).isEqualTo(ProjectStructureManager.RunType.CUCUMBER).perform();
    }

    @Test(description = "identifyRunType(suites) still detects a Cucumber TestNG runner class when the suite " +
            "restricts it to specific methods via <methods><include>, proving the #4078 class walk already " +
            "covers this declaration shape without needing a code change")
    public void identifyRunTypeWithSuitesDetectsCucumberTestNgRunnerClassFilteredByMethods() {
        XmlSuite suite = new XmlSuite();
        XmlTest xmlTest = new XmlTest(suite);
        XmlClass xmlClass = new XmlClass(cucumberTestRunner.CucumberTests.class);
        xmlClass.setIncludedMethods(List.of(new XmlInclude("runScenario")));
        xmlTest.setXmlClasses(List.of(xmlClass));

        ProjectStructureManager.RunType runType = ProjectStructureManager.identifyRunType(List.of(suite));

        SHAFT.Validations.assertThat().object(runType).isEqualTo(ProjectStructureManager.RunType.CUCUMBER).perform();
    }

    @Test(description = "identifyRunType(suites) terminates instead of infinite-looping when child suites form a " +
            "cycle, proving the child-suite recursion is bounded by a visited-set rather than plain depth-first " +
            "descent", timeOut = 10000)
    public void identifyRunTypeWithSuitesTerminatesOnCyclicChildSuites() {
        XmlSuite suiteA = new XmlSuite();
        XmlSuite suiteB = new XmlSuite();
        suiteA.getChildSuites().add(suiteB);
        suiteB.getChildSuites().add(suiteA); // cycle: B points back to A

        ProjectStructureManager.RunType runType = ProjectStructureManager.identifyRunType(List.of(suiteA));

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
