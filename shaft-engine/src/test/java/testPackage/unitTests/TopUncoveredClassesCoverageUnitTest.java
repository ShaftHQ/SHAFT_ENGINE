package testPackage.unitTests;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.validation.accessibility.AccessibilityHelper;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class TopUncoveredClassesCoverageUnitTest {
    private static final Path TEMP_DIR = Path.of("target", "temp", "topCoverage");

    @BeforeMethod(alwaysRun = true)
    public void setup() throws Exception {
        Files.createDirectories(TEMP_DIR);
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        FileActions.getInstance(true).deleteFolder(TEMP_DIR.toString());
        FileActions.getInstance(true).deleteFolder("accessibility-reports");
        FileActions.getInstance(true).deleteFolder("allure-results/accessibility");
    }

    @Test
    public void accessibilityHelperValidationPathsShouldBeCovered() {
        Assert.assertThrows(IllegalArgumentException.class,
                () -> AccessibilityHelper.analyzePageAccessibility(null, "page"));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> AccessibilityHelper.analyzePageAccessibility(mock(WebDriver.class), " "));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> AccessibilityHelper.analyzePageAccessibility(mock(WebDriver.class), "page", (AccessibilityHelper.AccessibilityConfig) null));

        Assert.assertThrows(IllegalArgumentException.class,
                () -> AccessibilityHelper.analyzePageAccessibilityAndSave(null, "page", false));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> AccessibilityHelper.analyzePageAccessibilityAndSave(mock(WebDriver.class), " ", false));

        Assert.assertFalse(AccessibilityHelper.isAccessible(null));
        Assert.assertThrows(RuntimeException.class, () -> AccessibilityHelper.hasCriticalViolations(null, "page"));
        Assert.assertThrows(RuntimeException.class, () -> AccessibilityHelper.getViolationsByType(null));
    }

    @Test
    public void accessibilityHelperUtilityAndReportMethodsShouldWork() throws Exception {
        AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig()
                .setTags(List.of("wcag21aa"))
                .setContext("main")
                .setIncludePasses(false)
                .setReportsDir(TEMP_DIR.resolve("a11y").toString() + File.separator);

        Assert.assertEquals(config.getTags(), List.of("wcag21aa"));
        Assert.assertEquals(config.getContext(), "main");
        Assert.assertFalse(config.isIncludePasses());

        AccessibilityHelper.AccessibilityResult result = new AccessibilityHelper.AccessibilityResult()
                .setPageName("Home")
                .setViolationsCount(1)
                .setPassesCount(2)
                .setScore(66.6)
                .setTimestamp("2026-01-01T00:00:00");
        Assert.assertTrue(result.hasViolations());
        Assert.assertTrue(result.toString().contains("Home"));

        Path reportDir = Path.of("accessibility-reports");
        Files.createDirectories(reportDir);
        Path oldReport = reportDir.resolve("AccessibilityReport_Home_old.html");
        Path newReport = reportDir.resolve("AccessibilityReport_Home_new.html");
        Files.writeString(oldReport, "old", StandardCharsets.UTF_8);
        Files.writeString(newReport, "new", StandardCharsets.UTF_8);
        Files.setLastModifiedTime(oldReport, FileTime.fromMillis(System.currentTimeMillis() - 10_000));
        Files.setLastModifiedTime(newReport, FileTime.fromMillis(System.currentTimeMillis()));

        Path latest = AccessibilityHelper.getLatestReportPath("Home");
        Assert.assertNotNull(latest);
        Assert.assertTrue(latest.getFileName().toString().contains("new"));

        AccessibilityHelper.attachReportToAllure("Home");

        Path filtered = TEMP_DIR.resolve("filtered.html");
        AccessibilityHelper.generateFilteredHTMLReport(result, "Home", filtered.toString(), null);
        Assert.assertTrue(Files.exists(filtered));
        Assert.assertTrue(Files.readString(filtered).contains("Accessibility Report"));

        AccessibilityHelper.attachFilteredReportToAllure("Home", result, null);

        Method wcagMapper = AccessibilityHelper.class.getDeclaredMethod("getWcagModelFromTags", List.class);
        wcagMapper.setAccessible(true);
        Assert.assertEquals(wcagMapper.invoke(null, List.of("wcag21aa")), "WCAG 2.1 A");
        Assert.assertEquals(wcagMapper.invoke(null, Collections.emptyList()), "Unknown");

        Method escapeHtml = AccessibilityHelper.class.getDeclaredMethod("escapeHtml", String.class);
        escapeHtml.setAccessible(true);
        Assert.assertEquals(escapeHtml.invoke(null, "<a&\"b'>"), "&lt;a&amp;&quot;b&#39;&gt;");
    }

    @Test
    public void elementActionsHelperSafeFindMethodsShouldHandleNormalAndFallbackPaths() {
        WebDriver driver = mock(WebDriver.class);
        WebElement element = mock(WebElement.class);

        when(driver.findElements(any(By.class))).thenReturn(List.of(element));
        when(driver.findElement(any(By.class))).thenReturn(element);

        List<WebElement> found = ElementActionsHelper.safeFindElements(driver, By.id("x"));
        Assert.assertEquals(found.size(), 1);
        Assert.assertNotNull(ElementActionsHelper.safeFindElement(driver, By.id("x")));

        WebDriver overflowDriver = mock(WebDriver.class);
        when(overflowDriver.findElements(any(By.class))).thenThrow(new StackOverflowError("forced"));
        when(overflowDriver.findElement(any(By.class))).thenThrow(new StackOverflowError("forced"));

        Assert.assertTrue(ElementActionsHelper.safeFindElements(overflowDriver, By.id("x")).isEmpty());
        Assert.assertThrows(NoSuchElementException.class,
                () -> ElementActionsHelper.safeFindElement(overflowDriver, By.id("x")));
    }

    @Test
    public void actionsNullGuardPathsShouldBeCovered() {
        WebDriver driver = mock(WebDriver.class);
        Actions actions = new Actions(driver, true);

        Assert.assertSame(actions.and(), actions);
        Assert.assertThrows(NullPointerException.class, () -> actions.click((By) null));
        Assert.assertThrows(NullPointerException.class, () -> actions.hover((By) null));
        Assert.assertThrows(NullPointerException.class, () -> actions.doubleClick((By) null));
        Assert.assertThrows(NullPointerException.class, () -> actions.type((By) null, "x"));
        Assert.assertThrows(NullPointerException.class, () -> actions.typeSecure((By) null, "x"));
        Assert.assertThrows(NullPointerException.class, () -> actions.typeAppend((By) null, "x"));
        Assert.assertThrows(NullPointerException.class, () -> actions.clear((By) null));
        Assert.assertThrows(NullPointerException.class, () -> actions.dropFileToUpload(null, "file.txt"));
        Assert.assertThrows(NullPointerException.class, () -> actions.click((String) null));
        Assert.assertThrows(NullPointerException.class, () -> actions.type((String) null, "x"));
    }

    @Test
    public void fileActionsTerminalAndListBranchesShouldBeCovered() throws Exception {
        FileActions fileActions = FileActions.getInstance(true);
        TerminalActions terminalActions = mock(TerminalActions.class);
        doReturn("ok").when(terminalActions).performTerminalCommand(any(String.class));
        doReturn("list").when(terminalActions).performTerminalCommands(anyList());

        String sourceDir = TEMP_DIR.resolve("source").toString();
        String destinationDir = TEMP_DIR.resolve("destination").toString();
        fileActions.createFolder(sourceDir);
        fileActions.createFolder(destinationDir);
        fileActions.writeToFile(Path.of(sourceDir, "a.txt").toString(), "A");

        String copyLog = fileActions.copyFile(terminalActions, sourceDir, destinationDir, "a.txt");
        Assert.assertEquals(copyLog, "ok");

        String listLog = fileActions.listFilesInDirectory(terminalActions, destinationDir);
        Assert.assertEquals(listLog, "list");

        String listed = fileActions.listFilesInDirectory(sourceDir);
        Assert.assertTrue(listed.contains("a.txt"));

        Assert.assertFalse(fileActions.getFileList(TEMP_DIR.resolve("missing").toString()).iterator().hasNext());

        Path src = TEMP_DIR.resolve("copy-source.txt");
        Path dst = TEMP_DIR.resolve("copy-destination.txt");
        Files.writeString(src, "copy", StandardCharsets.UTF_8);
        fileActions.copyFile(src.toString(), dst.toString());
        Assert.assertTrue(Files.exists(dst));
    }
}
