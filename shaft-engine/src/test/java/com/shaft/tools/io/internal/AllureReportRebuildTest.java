package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.apache.commons.lang3.SystemUtils;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

/**
 * Covers #7174: {@link AllureManager#generateReport(Path)} rebuilds the report from existing
 * results without cleaning them or rewriting {@code environment.xml}, missing attachment files are
 * reported, and a local {@code allure.customLogo} is inlined so single-file reports can show it.
 */
@Test(singleThreaded = true)
public class AllureReportRebuildTest {
    private static final String RESULT_JSON = """
            {"uuid":"r1","name":"t","status":"passed","attachments":[{"name":"shot","source":"present-attachment.png","type":"image/png"},{"name":"blank","source":""}],
             "steps":[{"name":"s","status":"passed","attachments":[{"name":"log","source":"missing-attachment.txt","type":"text/plain"}],"steps":[]}]}
            """;
    private static final String CONTAINER_JSON = """
            {"uuid":"c1","children":["r1"],"befores":[{"name":"setup","status":"passed","attachments":[{"name":"props","source":"missing-fixture.txt","type":"text/plain"}]}]}
            """;

    private Path root;
    private Path results;
    private String originalLogo;
    private String originalResultsFolder;
    private String originalPrefix;

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws Exception {
        root = Files.createTempDirectory("shaft-report-rebuild").toAbsolutePath().normalize();
        results = Files.createDirectories(root.resolve("merged-results"));
        Files.writeString(results.resolve("r1-result.json"), RESULT_JSON, StandardCharsets.UTF_8);
        Files.writeString(results.resolve("c1-container.json"), CONTAINER_JSON, StandardCharsets.UTF_8);
        Files.write(results.resolve("present-attachment.png"), new byte[]{1, 2, 3});
        Files.writeString(results.resolve("environment.xml"), "<environment>merged</environment>", StandardCharsets.UTF_8);
        originalLogo = SHAFT.Properties.allure.customLogo();
        originalResultsFolder = AllureManager.allureResultsFolder();
        originalPrefix = AllureManager.allureCommandPrefix();
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        SHAFT.Properties.allure.set().customLogo(originalLogo);
        AllureManager.allureResultsFolder(originalResultsFolder);
        AllureManager.allureCommandPrefix(originalPrefix);
        Properties.clearForCurrentThread();
        try (Stream<Path> paths = Files.walk(root)) {
            paths.sorted(Comparator.reverseOrder()).map(Path::toFile).forEach(File::delete);
        }
    }

    @Test
    public void missingAttachmentFilesAreListedFromResultsAndContainers() {
        Assert.assertEquals(AllureManager.findMissingAttachments(results),
                List.of("missing-attachment.txt", "missing-fixture.txt"));
    }

    @Test
    public void unreadableResultFilesAndNonDirectoriesYieldNoMissingAttachments() throws IOException {
        Files.writeString(results.resolve("broken-result.json"), "{not json", StandardCharsets.UTF_8);
        Assert.assertEquals(AllureManager.findMissingAttachments(results),
                List.of("missing-attachment.txt", "missing-fixture.txt"), "a broken result file is skipped");
        Assert.assertEquals(AllureManager.findMissingAttachments(results.resolve("r1-result.json")), List.of());
    }

    @Test
    public void logoMimeTypeFollowsTheExtension() throws IOException {
        byte[] bytes = {1, 2, 3};
        for (String[] pair : new String[][]{{"svg", "image/svg+xml"}, {"jpg", "image/jpeg"}, {"jpeg", "image/jpeg"},
                {"gif", "image/gif"}, {"webp", "image/webp"}}) {
            Path logo = Files.write(root.resolve("logo." + pair[0]), bytes);
            Assert.assertTrue(AllureManager.resolveReportLogo(logo.toString()).startsWith("data:" + pair[1] + ";base64,"),
                    pair[0]);
        }
    }

    @Test
    public void malformedFileUriLogoIsKept() {
        Assert.assertEquals(AllureManager.resolveReportLogo("file://remote-host/logo.png"), "file://remote-host/logo.png");
    }

    @Test
    public void localLogoIsInlinedAsDataUri() throws IOException {
        byte[] png = {(byte) 0x89, 'P', 'N', 'G', 1, 2};
        Path logo = Files.write(root.resolve("logo.png"), png);
        String expected = "data:image/png;base64," + Base64.getEncoder().encodeToString(png);

        Assert.assertEquals(AllureManager.resolveReportLogo("file:" + logo), expected);
        Assert.assertEquals(AllureManager.resolveReportLogo(logo.toUri().toString()), expected);
        Assert.assertEquals(AllureManager.resolveReportLogo(logo.toString()), expected);
    }

    @Test
    public void remoteAndUnreadableLogosAreKept() {
        String remote = "https://example.com/logo.png";
        Assert.assertEquals(AllureManager.resolveReportLogo(remote), remote);
        Assert.assertEquals(AllureManager.resolveReportLogo("data:image/png;base64,AA=="), "data:image/png;base64,AA==");
        Assert.assertEquals(AllureManager.resolveReportLogo("file:does/not/exist.png"), "file:does/not/exist.png");
        Assert.assertEquals(AllureManager.resolveReportLogo(""), "");
    }

    @Test
    public void generateReportRebuildsFromExistingResultsWithoutTouchingThem() throws Exception {
        if (SystemUtils.IS_OS_WINDOWS) {
            throw new SkipException("The stand-in Allure CLI is a POSIX shell script.");
        }
        byte[] png = {(byte) 0x89, 'P', 'N', 'G'};
        Path logo = Files.write(root.resolve("logo.png"), png);
        SHAFT.Properties.allure.set().customLogo("file:" + logo).singleFile(true).accumulateReports(false);
        // Stand-in CLI: "generate --config <cfg> <results> -o <out>" writes <out>/index.html with the config.
        Path cli = root.resolve("fake-allure.sh");
        Files.writeString(cli, "#!/bin/sh\nmkdir -p \"$6\" && { echo '<html><head></head><body><div id=\"app\"></div>'; cat \"$3\"; echo '</body></html>'; } > \"$6/index.html\"\n",
                StandardCharsets.UTF_8);
        AllureManager.allureCommandPrefix("sh \"" + cli + "\"");
        AllureManager.allureResultsFolder("unchanged-results/");

        Path report = SHAFT.Report.generateAllureReport(results);

        Assert.assertEquals(report, root.resolve("allure-report").resolve("AllureReport.html"));
        String html = Files.readString(report, StandardCharsets.UTF_8);
        Assert.assertTrue(html.contains("logo: \"data:image/png;base64,"), "single-file report carries the inlined logo");
        Assert.assertEquals(Files.readString(results.resolve("environment.xml"), StandardCharsets.UTF_8),
                "<environment>merged</environment>", "environment.xml is never rewritten");
        Assert.assertTrue(Files.exists(results.resolve("present-attachment.png")), "results are never cleaned");
        Assert.assertEquals(AllureManager.allureResultsFolder(), "unchanged-results/",
                "the in-run results path is restored");
    }

    @Test
    public void generateReportFailsLoudlyWhenNoReportIsProduced() {
        if (SystemUtils.IS_OS_WINDOWS) {
            throw new SkipException("The stand-in Allure CLI is a POSIX command.");
        }
        AllureManager.allureCommandPrefix("true");
        Assert.assertThrows(IllegalStateException.class, () -> AllureManager.generateReport(results));
    }

    @Test
    public void generateReportRejectsAMissingResultsDirectory() {
        Assert.assertThrows(IllegalArgumentException.class,
                () -> SHAFT.Report.generateAllureReport(root.resolve("absent")));
    }

}
