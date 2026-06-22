package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.cli.TerminalActions;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.mockito.Mockito;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@Test(singleThreaded = true)
public class AllureManagerCoverageUnitTest {
    private static final Path TEST_ROOT = Path.of("target", "allure-manager-coverage-unit");
    private Path testDirectory;
    private Path allureResultsDirectory;

    @BeforeMethod(alwaysRun = true)
    public void setUp(Method method) throws Exception {
        testDirectory = TEST_ROOT.resolve(method.getName());
        allureResultsDirectory = testDirectory.resolve("allure-results");
        deleteRecursively(testDirectory);
        Files.createDirectories(allureResultsDirectory);
        resetAllureManagerState();
        SHAFT.Properties.paths.set().allureResults(allureResultsDirectory + File.separator);
        SHAFT.Properties.allure.set()
                .accumulateReports(false)
                .automaticallyOpen(false)
                .cleanResultsDirectory(true)
                .forceConfiguredCliVersion(true)
                .generateArchive(false)
                .realtimeMonitoring(false)
                .singleFile(true)
                .open(false)
                .groupBy("parentSuite,suite,subSuite");
        setField("allureResultsFolderPath", allureResultsDirectory + File.separator);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        invoke("stopRealtimeMonitoring");
        resetAllureManagerState();
        setField("cachedAllureCommandPrefix", "");
        setField("allureResultsFolderPath", SHAFT.Properties.paths.allureResults());
        Properties.clearForCurrentThread();
        deleteRecursively(testDirectory);
        Files.deleteIfExists(Path.of("allurerc.yaml"));
        Files.deleteIfExists(Path.of("generate_allure_report.sh"));
        Files.deleteIfExists(Path.of("generate_allure_report.bat"));
        deleteRecursively(Path.of("allure-report"));
        deleteRecursively(Path.of("target", "allure-report"));
        Files.deleteIfExists(Path.of("target", "allure-watch.log"));
    }

    @Test
    public void getResultsPathShouldStripOnlyTrailingSeparators() throws Exception {
        setField("allureResultsFolderPath", allureResultsDirectory + File.separator);
        Assert.assertEquals(invoke("getResultsPath"), allureResultsDirectory.toString());

        Path pathWithoutTrailingSeparator = testDirectory.resolve("directory-name-without-separator");
        setField("allureResultsFolderPath", pathWithoutTrailingSeparator.toString());
        Assert.assertEquals(invoke("getResultsPath"), pathWithoutTrailingSeparator.toString());

        setField("allureResultsFolderPath", "");
        Assert.assertEquals(invoke("getResultsPath"), "");
    }

    @Test
    public void initializeAllureReportingEnvironmentShouldCreateScriptsAndEnvironmentFileWithoutResolvingCLI() throws Exception {
        setField("cachedAllureCommandPrefix", "");
        Files.createDirectories(allureResultsDirectory);
        Files.writeString(allureResultsDirectory.resolve("stale-result.txt"), "stale", StandardCharsets.UTF_8);

        AllureManager.initializeAllureReportingEnvironment();

        Path generatedReportScript = System.getProperty("os.name").toLowerCase().contains("win")
                ? Path.of("generate_allure_report.bat")
                : Path.of("generate_allure_report.sh");
        Assert.assertTrue(Files.exists(generatedReportScript), "Environment script should be generated.");
        Assert.assertTrue(Files.readString(generatedReportScript, StandardCharsets.UTF_8)
                .contains("npx --yes allure@" + SHAFT.Properties.internal.allure3Version()));
        Assert.assertFalse(Files.exists(allureResultsDirectory.resolve("stale-result.txt")),
                "initializeAllureReportingEnvironment should clean stale results entries.");
        Assert.assertTrue(Files.exists(allureResultsDirectory.resolve("environment.xml")));
    }

    @Test
    public void openAllureReportAfterExecutionShouldCopyReportAndCallViewerCommandWhenReportExists() throws Exception {
        setField("cachedAllureCommandPrefix", "");
        Path outputDirectory = (Path) invoke("reportOutputDirectoryPath");
        Path destinationDirectory = (Path) invoke("reportDirectoryPath");
        Files.createDirectories(outputDirectory);
        Files.writeString(outputDirectory.resolve("index.html"), "<html><body>Allure Report</body></html>");

        Object originalTerminalSession = getFieldValue("internalTerminalSession");
        TerminalActions terminalMock = Mockito.mock(TerminalActions.class);
        setField("internalTerminalSession", terminalMock);
        try {
            AllureManager.openAllureReportAfterExecution();

            Assert.assertTrue(Files.exists(destinationDirectory.resolve("AllureReport.html")),
                    "Generated allure report should be copied and renamed.");
            Assert.assertFalse(Files.exists(outputDirectory), "Temporary report output directory should be removed after copy.");
            Mockito.verify(terminalMock).performTerminalCommand(Mockito.argThat(argument ->
                    argument != null && argument.contains("AllureReport.html")));
        } finally {
            setField("internalTerminalSession", originalTerminalSession);
        }
    }

    @Test
    public void cleanAllureResultsDirectoryShouldCreateMissingRootAndDeleteOnlyContents() throws Exception {
        Path nestedDirectory = allureResultsDirectory.resolve("nested");
        Files.createDirectories(nestedDirectory);
        Files.writeString(nestedDirectory.resolve("stale.json"), "{}", StandardCharsets.UTF_8);
        Files.writeString(allureResultsDirectory.resolve("stale.txt"), "stale", StandardCharsets.UTF_8);

        invoke("cleanAllureResultsDirectory");

        Assert.assertTrue(Files.isDirectory(allureResultsDirectory));
        Assert.assertEquals(countChildren(allureResultsDirectory), 0L);

        deleteRecursively(allureResultsDirectory);
        Assert.assertFalse(Files.exists(allureResultsDirectory));

        invoke("cleanAllureResultsDirectory");

        Assert.assertTrue(Files.isDirectory(allureResultsDirectory));
        Assert.assertEquals(countChildren(allureResultsDirectory), 0L);
    }

    @Test
    public void cleanAllureResultsDirectoryShouldIgnoreMissingDirectoryWhenCleaningContentsDirectly() throws Exception {
        Path missingDirectory = testDirectory.resolve("missing-results");
        deleteRecursively(missingDirectory);

        invoke("cleanAllureResultsDirectoryContents", new Class[]{String.class}, missingDirectory.toString());
        invoke("ensureAllureResultsDirectoryExists", new Class[]{String.class}, "");

        Assert.assertFalse(Files.exists(missingDirectory));
    }

    @Test
    public void getCommandToCreateAllureReportShouldBuildAllure3AndAllure2Commands() throws Exception {
        Path outputDirectory = testDirectory.resolve("generated-report");
        setField("allureOutPutDirectory", outputDirectory.toString());
        setField("cachedAllureCommandPrefix", "allure");
        setField("cachedIsAllure2", false);

        String allure3Command = (String) invoke("getCommandToCreateAllureReport");

        Assert.assertTrue(allure3Command.startsWith("allure generate --config "), allure3Command);
        Assert.assertTrue(allure3Command.contains(quote(Path.of("allurerc.yaml").toAbsolutePath().toString())), allure3Command);
        Assert.assertTrue(allure3Command.contains(quote(allureResultsDirectory.toString())), allure3Command);
        Assert.assertTrue(allure3Command.endsWith(" -o " + quote(outputDirectory.toString())), allure3Command);

        setField("cachedIsAllure2", true);
        String allure2Command = (String) invoke("getCommandToCreateAllureReport");

        Assert.assertEquals(allure2Command,
                "allure generate " + quote(allureResultsDirectory.toString())
                        + " --single-file --clean -o " + quote(outputDirectory.toString()));
    }

    @Test
    public void writeGenerateReportShellFilesShouldUseConfiguredAllureVersionAndTrimResultsPath() throws Exception {
        setField("cachedIsAllure2", false);
        SHAFT.Properties.allure.set().forceConfiguredCliVersion(true);

        invoke("writeGenerateReportShellFilesToProjectDirectory");

        Path script = System.getProperty("os.name").toLowerCase().contains("win")
                ? Path.of("generate_allure_report.bat")
                : Path.of("generate_allure_report.sh");
        String scriptContents = Files.readString(script, StandardCharsets.UTF_8);

        Assert.assertTrue(scriptContents.contains("npx --yes allure@" + SHAFT.Properties.internal.allure3Version()), scriptContents);
        Assert.assertTrue(scriptContents.contains("serve --config "
                + quote(Path.of("allurerc.yaml").toAbsolutePath().toString())
                + " " + quote(allureResultsDirectory.toString())), scriptContents);
        Assert.assertFalse(scriptContents.contains(allureResultsDirectory + File.separator + "\""), scriptContents);
    }

    @Test
    public void writeAllureConfigShouldEscapeValuesAndUseDefaultGroupingWhenBlank() throws Exception {
        SHAFT.Properties.allure.set()
                .customLogo("logo\\path\"name")
                .customTitle("ignored")
                .groupBy("  ")
                .open(true)
                .reportLanguage("en\"GB")
                .singleFile(false);
        Path outputDirectory = testDirectory.resolve("report-output");

        invoke("writeAllureConfig", new Class[]{String.class, String.class}, "Report \"Name\"", outputDirectory.toString());

        String config = Files.readString(Path.of("allurerc.yaml"), StandardCharsets.UTF_8);
        Assert.assertTrue(config.contains("name: \"Report \\\"Name\\\"\""), config);
        Assert.assertTrue(config.contains("output: \"" + outputDirectory.toString().replace("\\", "/").replace("\"", "\\\"") + "\""), config);
        Assert.assertTrue(config.contains("logo: \"logo\\\\path\\\"name\""), config);
        Assert.assertTrue(config.contains("reportLanguage: \"en\\\"GB\""), config);
        Assert.assertTrue(config.contains("singleFile: false"), config);
        Assert.assertTrue(config.contains("open: true"), config);
        Assert.assertTrue(config.contains("        - parentSuite"), config);
        Assert.assertTrue(config.contains("        - suite"), config);
        Assert.assertTrue(config.contains("        - subSuite"), config);
    }

    @Test
    public void patchMissingStatusDetailsInResultsShouldPatchResultAndContainerFilesButPreserveAttachments() throws Exception {
        Path resultFile = allureResultsDirectory.resolve("sample-result.json");
        Files.writeString(resultFile, """
                {"name":"result","steps":[{"name":"step","steps":[{"name":"nested"}]}],"attachments":[{"name":"kept","source":"kept.txt"}]}
                """, StandardCharsets.UTF_8);
        Path containerFile = allureResultsDirectory.resolve("sample-container.json");
        Files.writeString(containerFile, """
                {"befores":[{"name":"empty"},{"name":"fixture","steps":[{"name":"fixture-step","statusDetails":{"trace":"trace"}}],"attachments":[{"name":"fixture-attachment"}]}],"afters":[{"name":"after","parameters":[{"name":"p"}]}]}
                """, StandardCharsets.UTF_8);
        Path ignoredFile = allureResultsDirectory.resolve("ignored.json");
        Files.writeString(ignoredFile, "{\"steps\":[{\"name\":\"ignored\"}]}", StandardCharsets.UTF_8);

        invoke("patchMissingStatusDetailsInResults", new Class[]{String.class}, allureResultsDirectory.toString());

        String patchedResult = Files.readString(resultFile, StandardCharsets.UTF_8);
        String patchedContainer = Files.readString(containerFile, StandardCharsets.UTF_8);
        String ignored = Files.readString(ignoredFile, StandardCharsets.UTF_8);

        Assert.assertTrue(patchedResult.contains("\"statusDetails\""), patchedResult);
        Assert.assertTrue(patchedResult.contains("\"message\":\"\""), patchedResult);
        Assert.assertTrue(patchedResult.contains("\"attachments\":[{\"name\":\"kept\",\"source\":\"kept.txt\"}]"), patchedResult);
        Assert.assertFalse(patchedContainer.contains("\"name\":\"empty\""), patchedContainer);
        Assert.assertTrue(patchedContainer.contains("\"trace\":\"trace\",\"message\":\"\""), patchedContainer);
        Assert.assertTrue(patchedContainer.contains("\"attachments\":[{\"name\":\"fixture-attachment\"}]"), patchedContainer);
        Assert.assertEquals(ignored, "{\"steps\":[{\"name\":\"ignored\"}]}");
    }

    @Test
    public void renameAllureReportShouldReturnNullWhenIndexIsMissingAndRenameWhenPresent() throws Exception {
        Path reportDirectory = Path.of("allure-report");
        deleteRecursively(reportDirectory);

        Assert.assertNull(invoke("renameAllureReport"));

        Files.createDirectories(reportDirectory);
        Files.writeString(reportDirectory.resolve("index.html"), "<html></html>", StandardCharsets.UTF_8);
        SHAFT.Properties.allure.set().accumulateReports(false);

        Assert.assertEquals(invoke("renameAllureReport"), "AllureReport.html");
        Assert.assertFalse(Files.exists(reportDirectory.resolve("index.html")));
        Assert.assertTrue(Files.exists(reportDirectory.resolve("AllureReport.html")));
    }

    @Test
    public void executeAllureGenerateCommandShouldHandleFailingAndMissingCommandsWithoutThrowing() throws Exception {
        invoke("executeAllureGenerateCommand", new Class[]{String.class}, "printf 'allure stdout'; printf 'allure stderr' >&2; exit 7");
        invoke("executeAllureGenerateCommand", new Class[]{String.class}, "definitely_missing_allure_command_for_coverage");
    }

    @Test
    public void qShouldEscapePathSeparatorsAndQuotationMarks() throws Exception {
        String quoted = (String) invoke("q", new Class[]{String.class}, "C:\\tmp\\report\"path");
        Assert.assertEquals(quoted, "\"C:\\\\tmp\\\\report\\\"path\"");

        String quotedNull = (String) invoke("q", new Class[]{String.class}, (Object) null);
        Assert.assertEquals(quotedNull, "\"\"");
    }

    @Test
    public void getCompletedProcessOutputShouldHandleSuccessExecutionFailureAndInterruption() throws Exception {
        CompletableFuture<String> completed = CompletableFuture.completedFuture("output");
        Assert.assertEquals(invoke("getCompletedProcessOutput", new Class[]{java.util.concurrent.Future.class, String.class}, completed, "stdout"), "output");

        CompletableFuture<String> failed = new CompletableFuture<>();
        failed.completeExceptionally(new ExecutionException("boom", new RuntimeException("root")));
        Assert.assertEquals(invoke("getCompletedProcessOutput", new Class[]{java.util.concurrent.Future.class, String.class}, failed, "stderr"), "");

        CompletableFuture<String> interrupted = new CompletableFuture<>() {
            @Override
            public String get() throws InterruptedException {
                throw new InterruptedException("interrupted");
            }
        };
        Assert.assertEquals(invoke("getCompletedProcessOutput", new Class[]{java.util.concurrent.Future.class, String.class}, interrupted, "stdout"), "");
        Assert.assertTrue(Thread.currentThread().isInterrupted());
        Thread.interrupted();
    }

    @Test
    public void generateAllureReportArchiveShouldSkipWhenDisabledAndCreateArchiveWhenEnabledWithoutCli() throws Exception {
        SHAFT.Properties.allure.set().generateArchive(false);
        AllureManager.generateAllureReportArchive();
        Assert.assertEquals(countMatchingFiles("generatedReport_", ".zip"), 0L);

        setField("cachedAllureCommandPrefix", "");
        SHAFT.Properties.allure.set().generateArchive(true);
        AllureManager.generateAllureReportArchive();

        Assert.assertTrue(countMatchingFiles("generatedReport_", ".zip") >= 1L);
        deleteGeneratedArchives();
    }

    private static Object invoke(String methodName, Object... args) throws Exception {
        Class<?>[] parameterTypes = new Class<?>[args.length];
        for (int i = 0; i < args.length; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        return invoke(methodName, parameterTypes, args);
    }

    private static Object invoke(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
        Method method = AllureManager.class.getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        try {
            return method.invoke(null, args);
        } catch (InvocationTargetException e) {
            if (e.getCause() instanceof Exception exception) {
                throw exception;
            }
            throw e;
        }
    }

    private static void setField(String fieldName, Object value) throws Exception {
        Field field = AllureManager.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        Object fieldValue = normalizeFieldValue(field.getType(), value);

        try {
            if (Modifier.isFinal(field.getModifiers())) {
                disableFinalModifier(field);
            }
            field.set(null, fieldValue);
            return;
        } catch (IllegalAccessException | IllegalArgumentException ignored) {
            // keep deterministic behavior if reflective write fails under JPMS access rules
        }
        throw new UnsupportedOperationException("Unable to set static field through reflection: " + fieldName);
    }

    private static Object normalizeFieldValue(Class<?> targetType, Object value) {
        if (value != null || !targetType.isPrimitive()) {
            return value;
        }
        return switch (targetType.getTypeName()) {
            case "int" -> 0;
            case "long" -> 0L;
            case "boolean" -> false;
            case "byte" -> (byte) 0;
            case "short" -> (short) 0;
            case "char" -> (char) 0;
            case "float" -> 0.0f;
            case "double" -> 0.0d;
            default -> false;
        };
    }

    @SuppressWarnings("PMD.AvoidAccessibilityAlteration")
    private static void disableFinalModifier(Field field) {
        try {
            Field modifiersField = Field.class.getDeclaredField("modifiers");
            modifiersField.setAccessible(true);
            modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
        } catch (NoSuchFieldException | IllegalAccessException ignored) {
            // JDK 12+ may hide Field#modifiers; best effort fallback keeps behavior stable for non-final fields.
        }
    }

    private static Object getFieldValue(String fieldName) throws Exception {
        Field field = AllureManager.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.get(null);
    }

    private static void resetAllureManagerState() throws Exception {
        setField("cachedAllureCommandPrefix", null);
        setField("cachedIsAllure2", false);
        setField("allureResultsFolderPath", "");
        setField("allureOutPutDirectory", "");
        setField("realtimeMonitoringProcess", null);
    }

    private static String quote(String value) {
        return "\"" + value + "\"";
    }

    private static long countChildren(Path directory) throws IOException {
        try (var children = Files.list(directory)) {
            return children.count();
        }
    }

    private static long countMatchingFiles(String prefix, String suffix) throws IOException {
        try (var files = Files.list(Path.of("."))) {
            return files
                    .map(Path::getFileName)
                    .map(Path::toString)
                    .filter(name -> name.startsWith(prefix) && name.endsWith(suffix))
                    .count();
        }
    }

    private static void deleteGeneratedArchives() throws IOException {
        try (var files = Files.list(Path.of("."))) {
            for (Path file : files
                    .filter(path -> path.getFileName().toString().startsWith("generatedReport_"))
                    .filter(path -> path.getFileName().toString().endsWith(".zip"))
                    .toList()) {
                Files.deleteIfExists(file);
            }
        }
    }

    private static void deleteRecursively(Path path) throws IOException {
        if (path == null || !Files.exists(path)) {
            return;
        }
        try (var paths = Files.walk(path)) {
            for (Path currentPath : paths.sorted(Comparator.reverseOrder()).toList()) {
                Files.deleteIfExists(currentPath);
            }
        }
    }
}
