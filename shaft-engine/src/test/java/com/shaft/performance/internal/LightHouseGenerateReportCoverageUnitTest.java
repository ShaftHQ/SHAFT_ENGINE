package com.shaft.performance.internal;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.net.MalformedURLException;
import java.net.URI;
import java.time.LocalDate;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class LightHouseGenerateReportCoverageUnitTest {
    private static final String AMPERSAND_PLACEHOLDER = "N898";

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        SHAFT.Properties.performance.set().isEnabled(false);
        SHAFT.Properties.reporting.set().openLighthouseReportWhileExecution(false);
        Properties.clearForCurrentThread();
    }

    @Test
    public void generateLightHouseReportShouldSkipExecutionWhenFeatureIsDisabled() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/home");
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver);

        SHAFT.Properties.performance.set().isEnabled(false);

        try (MockedStatic<FileActions> mockedFileActionsStatic = Mockito.mockStatic(FileActions.class);
             MockedConstruction<TerminalActions> mockedTerminalActions = Mockito.mockConstruction(TerminalActions.class)) {
            reportGenerator.generateLightHouseReport();

            mockedFileActionsStatic.verifyNoInteractions();
            Assert.assertTrue(mockedTerminalActions.constructed().isEmpty());
        }
    }

    @Test
    public void generateLightHouseReportShouldCreateArtifactsAndExecuteCommandsWhenEnabled() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/search?q=shaft&lang=en");
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver);

        SHAFT.Properties.performance.set().isEnabled(true);
        SHAFT.Properties.performance.set().port(9999);
        SHAFT.Properties.reporting.set().openLighthouseReportWhileExecution(true);

        try (MockedStatic<FileActions> mockedFileActionsStatic = Mockito.mockStatic(FileActions.class);
             MockedConstruction<TerminalActions> mockedTerminalActions = Mockito.mockConstruction(TerminalActions.class,
                     (mock, context) -> when(mock.performTerminalCommand(anyString())).thenReturn("ok"))) {
            FileActions mockedFileActions = mock(FileActions.class);
            when(mockedFileActions.readFile(anyString())).thenReturn("<html>report</html>");
            mockedFileActionsStatic.when(() -> FileActions.getInstance(true)).thenReturn(mockedFileActions);

            reportGenerator.generateLightHouseReport();

            verify(mockedFileActions, times(1)).createFolder("lighthouse-reports");
            verify(mockedFileActions, times(1)).writeToFile(Mockito.eq(""), Mockito.eq("GenerateLHScript.js"), any(List.class));
            verify(mockedFileActions, times(1)).writeToFile(Mockito.eq(""), Mockito.eq("OpenLHReport.js"), any(List.class));
            verify(mockedFileActions, times(1)).readFile(Mockito.argThat(path -> path.startsWith("lighthouse-reports/") && path.endsWith(".html")));

            Assert.assertEquals(mockedTerminalActions.constructed().size(), 2);
            verify(mockedTerminalActions.constructed().getFirst(), times(1))
                    .performTerminalCommand(Mockito.argThat(command ->
                            command.contains("node GenerateLHScript.js")
                                    && command.contains("--port=9999")
                                    && command.contains(AMPERSAND_PLACEHOLDER)));
            verify(mockedTerminalActions.constructed().get(1), times(1))
                    .performTerminalCommand(Mockito.argThat(command -> command.contains("node OpenLHReport.js")));
        }
    }

    @Test
    public void helperMethodsShouldWriteExpectedScriptsAndGenerateReadablePageName() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/path/sub-path");
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver);
        @SuppressWarnings("unchecked")
        List<String>[] openScriptContent = new List[1];
        @SuppressWarnings("unchecked")
        List<String>[] generateScriptContent = new List[1];

        try (MockedStatic<FileActions> mockedFileActionsStatic = Mockito.mockStatic(FileActions.class)) {
            FileActions mockedFileActions = mock(FileActions.class);
            mockedFileActionsStatic.when(() -> FileActions.getInstance(true)).thenReturn(mockedFileActions);
            doAnswer(invocation -> {
                openScriptContent[0] = invocation.getArgument(2);
                return null;
            }).when(mockedFileActions).writeToFile(eq(""), eq("OpenLHReport.js"), Mockito.<List<String>>any());
            doAnswer(invocation -> {
                generateScriptContent[0] = invocation.getArgument(2);
                return null;
            }).when(mockedFileActions).writeToFile(eq(""), eq("GenerateLHScript.js"), Mockito.<List<String>>any());

            reportGenerator.createLighthouseReportFolderInProjectDirectory();
            reportGenerator.writeReportPathToFilesInProjectDirectory("custom-page");
            reportGenerator.writeNodeScriptFileInProjectDirectory();
            String pageName = reportGenerator.getPageName();

            verify(mockedFileActions, times(1)).createFolder("lighthouse-reports");
            verify(mockedFileActions, times(1)).writeToFile(eq(""), eq("OpenLHReport.js"), Mockito.<List<String>>any());
            verify(mockedFileActions, times(1)).writeToFile(eq(""), eq("GenerateLHScript.js"), Mockito.<List<String>>any());
            Assert.assertTrue(openScriptContent[0].get(0).contains("custom-page.html"));
            Assert.assertTrue(generateScriptContent[0].get(0).contains("desktop-config.js"));

            Assert.assertTrue(pageName.contains("--path-sub-path"));
            Assert.assertTrue(pageName.startsWith(LocalDate.now().format(java.time.format.DateTimeFormatter.ofPattern("dd-MM-yyyy"))));
        }
    }

    @Test
    public void getPageNameShouldReturnFallbackMessageWhenUrlConversionFails() throws MalformedURLException {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/invalid");
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver);

        URI mockedUri = mock(URI.class);
        when(mockedUri.toURL()).thenThrow(new MalformedURLException("forced"));

        try (MockedStatic<URI> mockedUriStatic = Mockito.mockStatic(URI.class)) {
            mockedUriStatic.when(() -> URI.create("https://example.com/invalid")).thenReturn(mockedUri);

            Assert.assertEquals(reportGenerator.getPageName(), "Error Occurred while creating the requested page name");
        }
    }
}
