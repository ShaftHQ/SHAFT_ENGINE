package com.shaft.cli;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import external.FileActionsReflectionInvoker;
import org.apache.commons.io.filefilter.TrueFileFilter;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class FileActionsCoverageUnitTest {
    private Path tempDirectory;

    @BeforeMethod
    public void createTempDirectory() throws IOException {
        tempDirectory = Files.createTempDirectory("shaft-file-actions-");
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws IOException {
        Properties.clearForCurrentThread();
        if (tempDirectory != null && Files.exists(tempDirectory)) {
            try (var paths = Files.walk(tempDirectory)) {
                paths.sorted(Comparator.reverseOrder())
                        .forEach(path -> {
                            try {
                                Files.deleteIfExists(path);
                            } catch (IOException ignored) {
                                path.toFile().deleteOnExit();
                            }
                        });
            }
        }
    }

    @Test
    public void instanceCreationAndPathHelpersShouldReturnUsableInstances() throws IOException {
        FileActions publicActions = FileActions.getInstance();
        FileActions internalActions = FileActions.getInstance(true);
        Path target = tempDirectory.resolve("path-helper.txt");
        Files.writeString(target, "path-helper", StandardCharsets.UTF_8);

        String absoluteFromSinglePath = publicActions.getAbsolutePath(target.toString());
        String absoluteFromFolderAndName = internalActions.getAbsolutePath(tempDirectory.toString() + '/', "path-helper.txt");

        Assert.assertNotNull(publicActions);
        Assert.assertNotNull(internalActions);
        Assert.assertEquals(absoluteFromSinglePath, target.toAbsolutePath().toString());
        Assert.assertEquals(absoluteFromFolderAndName, target.toAbsolutePath().toString());
    }

    @Test
    public void createWriteAppendReadListCopyRenameAndDeleteShouldOperateInsideTempDirectory() throws IOException {
        FileActions actions = FileActions.getInstance();
        Path sourceFolder = tempDirectory.resolve("source");
        Path nestedFolder = sourceFolder.resolve("nested");
        String sourceFolderPrefix = sourceFolder + java.io.File.separator;
        String nestedFolderPrefix = nestedFolder + java.io.File.separator;

        actions.createFolder(nestedFolder.toString());
        actions.createFile(sourceFolderPrefix, "alpha.txt");
        actions.writeToFile(sourceFolderPrefix, "alpha.txt", List.of("first", "second"));
        actions.writeToFile(nestedFolderPrefix, "bytes.bin", "bytes".getBytes(StandardCharsets.UTF_8));
        actions.writeToFile(nestedFolder.resolve("string.txt").toString(), "string-content");
        actions.writeToFile(nestedFolder.resolve("raw.bin").toString(), "raw".getBytes(StandardCharsets.UTF_8));

        Path alpha = sourceFolder.resolve("alpha.txt");
        Path alphaCopy = sourceFolder.resolve("alpha-copy.txt");
        actions.copyFile(alpha.toString(), alphaCopy.toString());
        actions.renameFile(alphaCopy.toString(), "renamed.txt");
        actions.renameFile(sourceFolder.resolve("renamed.txt").toString(), "renamed.txt");

        Assert.assertTrue(actions.doesFileExist(sourceFolderPrefix, "alpha.txt", 1));
        Assert.assertTrue(actions.doesFileExist(alpha.toString()));
        Assert.assertFalse(actions.doesFileExist(sourceFolder.resolve("missing.txt").toString()));
        Assert.assertFalse(actions.doesFileExist(sourceFolderPrefix, "missing.txt", 0));
        Assert.assertEquals(actions.readFile(sourceFolderPrefix, "alpha.txt"), String.join(System.lineSeparator(), "first", "second"));
        Assert.assertEquals(new String(actions.readFileAsByteArray(nestedFolder.resolve("bytes.bin").toString()), StandardCharsets.UTF_8), "bytes");
        String recursiveList = actions.listFilesInDirectory(sourceFolder.toString());
        String topLevelList = actions.listFilesInDirectory(sourceFolder.toString(), (TrueFileFilter) null);
        Assert.assertTrue(recursiveList.contains("alpha.txt"));
        Assert.assertTrue(recursiveList.contains("bytes.bin"));
        Assert.assertTrue(topLevelList.contains("alpha.txt"));
        Assert.assertFalse(topLevelList.contains("bytes.bin"));
        Assert.assertEquals(actions.getFileList(sourceFolder.toString()).size(), 5);

        Path destinationFolder = tempDirectory.resolve("destination");
        actions.copyFolder(sourceFolder.toString(), destinationFolder.toString());
        Assert.assertTrue(Files.exists(destinationFolder.resolve("nested/string.txt")));

        actions.deleteFile(sourceFolder.resolve("renamed.txt").toString());
        actions.deleteFolder(destinationFolder.toString());
        actions.deleteFile(sourceFolder.resolve("already-missing.txt").toString());
        Assert.assertFalse(Files.exists(sourceFolder.resolve("renamed.txt")));
        Assert.assertFalse(Files.exists(destinationFolder));
    }

    @Test
    public void terminalBackedMethodsShouldBuildExpectedLocalCommandsAndChecksums() throws IOException {
        FileActions actions = FileActions.getInstance(true);
        TerminalActions terminal = Mockito.mock(TerminalActions.class);
        Mockito.when(terminal.performTerminalCommand(Mockito.anyString())).thenReturn("copied");
        Mockito.when(terminal.performTerminalCommands(Mockito.anyList())).thenReturn("listed");
        SHAFT.Properties.platform.set().executionAddress("remote-grid").targetPlatform("LINUX");
        Path checksumFile = tempDirectory.resolve("checksum.txt");
        Files.writeString(checksumFile, "checksum", StandardCharsets.UTF_8);

        String namedCopyLog = actions.copyFile(terminal, tempDirectory.toString(), tempDirectory.resolve("out").toString(), "checksum.txt");
        String folderCopyLog = actions.copyFile(terminal, tempDirectory.toString(), tempDirectory.resolve("out2").toString(), " ");
        String listLog = actions.listFilesInDirectory(terminal, tempDirectory.toString());
        String copiedLocalPath = actions.copyFileToLocalMachine(terminal, tempDirectory.toString() + java.io.File.separator, "checksum.txt");
        String checksum = actions.getFileChecksum(terminal, tempDirectory.toString() + java.io.File.separator, "checksum.txt");

        ArgumentCaptor<String> commandCaptor = ArgumentCaptor.forClass(String.class);
        Mockito.verify(terminal, Mockito.times(2)).performTerminalCommand(commandCaptor.capture());
        Assert.assertTrue(commandCaptor.getAllValues().get(0).contains("rsync --verbose --recursive"));
        Assert.assertTrue(commandCaptor.getAllValues().get(0).contains("checksum.txt"));
        Assert.assertTrue(commandCaptor.getAllValues().get(1).contains("rsync --verbose --recursive"));
        Assert.assertEquals(namedCopyLog, "copied");
        Assert.assertEquals(folderCopyLog, "copied");
        Assert.assertEquals(listLog, "listed");
        Assert.assertEquals(copiedLocalPath, checksumFile.toString());
        Assert.assertEquals(checksum, "96fa8f226d3801741e807533552bc4b177ac4544d834073b6a5298934d34b40b");
        Assert.expectThrows(RuntimeException.class, () -> actions.getFileChecksum(terminal, tempDirectory.toString() + java.io.File.separator, "missing-checksum.txt"));

        SHAFT.Properties.platform.set().executionAddress("remote-grid").targetPlatform("WINDOWS");
        actions.copyFile(terminal, "C:/source", "C:/destination", "file.txt");
        actions.listFilesInDirectory(terminal, "C:/source");
        SHAFT.Properties.platform.set().targetPlatform("LINUX");
        actions.copyFile(terminal, "/source", "/destination", "file.txt");
        actions.listFilesInDirectory(terminal, "/source");
        SHAFT.Properties.platform.set().targetPlatform("SOLARIS");
        actions.copyFile(terminal, "/source", "/destination", "file.txt");
    }

    @Test
    public void remoteDockerCopyToLocalMachineShouldUseTempDirectoryWhenStaticHelpersAreStubbed() {
        FileActions realActions = FileActions.getInstance(true);
        FileActions helperActions = Mockito.spy(FileActions.getInstance(true));
        Path localTemp = tempDirectory.resolve("local-temp");
        Mockito.doReturn(localTemp.toString()).when(helperActions).getAbsolutePath("target/temp");
        Mockito.doReturn(tempDirectory.resolve("id_rsa").toString()).when(helperActions).getAbsolutePath("keys/", "id_rsa");

        TerminalActions remoteDockerTerminal = Mockito.mock(TerminalActions.class);
        Mockito.when(remoteDockerTerminal.isDockerizedTerminal()).thenReturn(true);
        Mockito.when(remoteDockerTerminal.isRemoteTerminal()).thenReturn(true);
        Mockito.when(remoteDockerTerminal.getSshHostName()).thenReturn("remote-host");
        Mockito.when(remoteDockerTerminal.getSshPortNumber()).thenReturn(2222);
        Mockito.when(remoteDockerTerminal.getSshUsername()).thenReturn("user");
        Mockito.when(remoteDockerTerminal.getSshKeyFileFolderName()).thenReturn("keys/");
        Mockito.when(remoteDockerTerminal.getSshKeyFileName()).thenReturn("id_rsa");
        Mockito.when(remoteDockerTerminal.getDockerName()).thenReturn("container");

        try (var ignoredConstruction = Mockito.mockConstruction(TerminalActions.class,
                (mock, context) -> Mockito.when(mock.performTerminalCommand(Mockito.anyString())).thenReturn("ok"));
             var fileActionsStatic = Mockito.mockStatic(FileActions.class, Mockito.CALLS_REAL_METHODS)) {
            fileActionsStatic.when(() -> FileActions.getInstance(true)).thenReturn(helperActions);

            String localPath = realActions.copyFileToLocalMachine(remoteDockerTerminal, "/var/log/", "app.log", "/remote-temp/");

            Assert.assertEquals(Path.of(localPath), localTemp.resolve("app.log"));
            Assert.assertTrue(Files.isDirectory(localTemp));
        }
    }

    @Test
    public void copyFileToLocalMachineShouldSanitizeStrictHostKeyCheckingInScpCommandForRemoteTerminals() throws Exception {
        FileActions realActions = FileActions.getInstance(true);
        FileActions helperActions = Mockito.spy(FileActions.getInstance(true));
        List<String> terminalCommands = new java.util.ArrayList<>();
        String localTempFolder = tempDirectory.resolve("local-temp-remote").toString();

        Mockito.doReturn(localTempFolder).when(helperActions).getAbsolutePath("target/temp");
        Mockito.doReturn(tempDirectory.resolve("keys/id_rsa").toString()).when(helperActions)
                .getAbsolutePath("keys/", "id_rsa");

        TerminalActions remoteTerminal = new TerminalActions(
                "host.example.com",
                2222,
                "user",
                "keys/",
                "id_rsa");

        String originalStrictMode = System.getProperty("shaft.cli.strictHostKeyChecking");
        try {
            System.setProperty("shaft.cli.strictHostKeyChecking", "ask; rm -rf /");
            try (var terminalConstructions = Mockito.mockConstruction(TerminalActions.class,
                    (mock, context) -> Mockito.when(mock.performTerminalCommand(Mockito.anyString())).thenAnswer(
                            invocation -> {
                                terminalCommands.add((String) invocation.getArgument(0));
                                return "copied";
                            }));
                 var fileActionsStatic = Mockito.mockStatic(FileActions.class, Mockito.CALLS_REAL_METHODS)) {
                fileActionsStatic.when(() -> FileActions.getInstance(true)).thenReturn(helperActions);

                String localPath = realActions.copyFileToLocalMachine(remoteTerminal,
                        "/var/log/",
                        "app.log",
                        "/remote-temp/");

                Assert.assertEquals(terminalCommands.size(), 2,
                        "Expected chmod and scp commands for remote copy flow.");
                Assert.assertTrue(localPath.startsWith(Path.of(localTempFolder).resolve("app.log").toString()));
                Assert.assertTrue(terminalCommands.get(1).contains("-o StrictHostKeyChecking=ask"),
                        terminalCommands.get(1));
                Assert.assertTrue(terminalCommands.get(1).contains("-r user@host.example.com:/var/log/app.log"),
                        terminalCommands.get(1));
            }
        } finally {
            if (originalStrictMode == null) {
                System.clearProperty("shaft.cli.strictHostKeyChecking");
            } else {
                System.setProperty("shaft.cli.strictHostKeyChecking", originalStrictMode);
            }
        }
    }

    @SuppressWarnings("PMD.AvoidAccessibilityAlteration")
    @Test
    public void normalizeStrictHostKeyCheckingModeShouldNormalizeAndValidateValues() throws Exception {
        Method normalize = FileActions.class.getDeclaredMethod("normalizeStrictHostKeyCheckingMode", String.class);
        normalize.setAccessible(true);
        Assert.assertEquals(normalize.invoke(null, "YES"), "yes");
        Assert.assertEquals(normalize.invoke(null, "  no  "), "no");
        Assert.assertEquals(normalize.invoke(null, "AcCePt-NeW"), "accept-new");
        Assert.assertEquals(normalize.invoke(null, "invalid"), "ask");
        Assert.assertEquals(normalize.invoke(null, ""), "ask");
        Assert.assertEquals(normalize.invoke(null, (Object) null), "ask");
    }

    @Test
    public void zipUnpackDownloadAndJarCopyShouldRoundTripTempFiles() throws Exception {
        FileActions actions = FileActions.getInstance();
        Path archiveSource = tempDirectory.resolve("archive-source");
        Path child = archiveSource.resolve("child");
        Path empty = archiveSource.resolve("empty");
        Files.createDirectories(child);
        Files.createDirectories(empty);
        Files.writeString(child.resolve("payload.txt"), "payload", StandardCharsets.UTF_8);
        Path zip = tempDirectory.resolve("archive.zip");
        SHAFT.Properties.reporting.set().debugMode(true);

        Assert.assertTrue(actions.zipFiles(archiveSource.toString(), zip.toString()));
        Assert.assertTrue(zipContains(zip, "payload.txt"));
        Assert.assertTrue(zipContains(zip, "empty"));

        Path unpacked = tempDirectory.resolve("unpacked");
        Assert.assertEquals(actions.unpackArchive(zip.toUri().toURL(), unpacked.toString()).getParentFile().toPath(), unpacked);
        Assert.assertTrue(Files.exists(unpacked.resolve("archive-source/child/payload.txt")));

        Path downloaded = tempDirectory.resolve("downloaded.zip");
        URL downloadedUrl = actions.downloadFile(zip.toUri().toURL().toString(), downloaded.toString());
        Assert.assertNotNull(downloadedUrl);
        Assert.assertTrue(Files.exists(downloaded));

        Path maliciousZip = createZipWithTraversalEntry();
        Assert.expectThrows(RuntimeException.class, () -> actions.unpackArchive(maliciousZip.toUri().toURL(), tempDirectory.resolve("malicious-unpack").toString()));
        Path zipDestinationDirectory = tempDirectory.resolve("zip-destination-directory");
        Files.createDirectory(zipDestinationDirectory);
        Assert.expectThrows(RuntimeException.class, () -> actions.zipFiles(archiveSource.toString(), zipDestinationDirectory.toString()));

        Path blockedDestinationParent = tempDirectory.resolve("blocked-destination-parent");
        Files.writeString(blockedDestinationParent, "file blocks child directory", StandardCharsets.UTF_8);
        Assert.expectThrows(RuntimeException.class, () -> actions.unpackArchive(zip.toUri().toURL(), blockedDestinationParent.resolve("child").toString()));

        Path jar = createJarWithResources();
        Path jarFolderDestination = tempDirectory.resolve("jar-folder");
        Path jarFileDestination = tempDirectory.resolve("jar-file");
        Files.createDirectories(jarFolderDestination);
        Files.createDirectories(jarFileDestination);
        String jarResourceUrl = jar.toUri().toURL() + "!/assets/";
        actions.copyFolderFromJar(jarResourceUrl, jarFolderDestination.toString());
        actions.copyFileFromJar(jarResourceUrl, jarFileDestination.toString(), "one.txt");
        Assert.assertEquals(Files.readString(jarFolderDestination.resolve("one.txt")), "one");
        Assert.assertEquals(Files.readString(jarFolderDestination.resolve("nested/two.txt")), "two");
        Assert.assertEquals(Files.readString(jarFileDestination.resolve("one.txt")), "one");

        Path jarDirectoryOnlyDestination = tempDirectory.resolve("jar-directory-only");
        Files.createDirectories(jarDirectoryOnlyDestination);
        actions.copyFileFromJar(jarResourceUrl, jarDirectoryOnlyDestination.toString(), "nested");
        Assert.assertTrue(Files.exists(jarDirectoryOnlyDestination.resolve("nested")));

        Path traversalJar = createJarWithTraversalEntry();
        String traversalJarResourceUrl = traversalJar.toUri().toURL() + "!/assets/";
        Assert.expectThrows(RuntimeException.class, () -> actions.copyFolderFromJar(traversalJarResourceUrl, tempDirectory.resolve("bad-jar-folder").toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.copyFileFromJar(traversalJarResourceUrl, tempDirectory.resolve("bad-jar-file").toString(), "evil.txt"));
    }

    @Test
    public void pdfAndLongReportDataPathsShouldUseFileActionsOverloads() throws IOException {
        FileActions actions = FileActions.getInstance();
        Path pdf = tempDirectory.resolve("sample.pdf");
        try (PDDocument document = new PDDocument()) {
            document.addPage(new PDPage());
            document.save(pdf.toFile());
        }

        Assert.assertEquals(actions.readPDF(pdf.toString()), "");
        Assert.assertEquals(actions.readPDF(tempDirectory.toString() + java.io.File.separator, "sample.pdf"), "");

        Path deepPath = tempDirectory;
        for (int i = 0; i < 35; i++) {
            deepPath = deepPath.resolve("segment" + i);
        }
        actions.createFolder(deepPath.toString());
        Assert.assertTrue(Files.isDirectory(deepPath));
    }

    @Test
    public void privateReportingBranchesShouldFormatShortLongPassAndFailMessages() throws Exception {
        FileActions actions = FileActions.getInstance();
        Method reportActionResult = FileActions.class.getDeclaredMethod(
                "reportActionResult", String.class, String.class, String.class, Boolean.class, Exception[].class);
        reportActionResult.setAccessible(true);
        String longTestData = "x".repeat(600);

        String passMessage = (String) reportActionResult.invoke(actions, "customAction", "short-data", "", true, new Exception[0]);
        String failMessage = (String) reportActionResult.invoke(actions, "customAction", longTestData, "diagnostic-log", false, new Exception[]{new IOException("boom")});

        Assert.assertTrue(passMessage.contains("completed"));
        Assert.assertTrue(failMessage.contains("failed"));

        Assert.assertTrue(FileActionsReflectionInvoker.invokePassReport(actions).contains("completed"));

        Method copyInputStream = FileActions.class.getDeclaredMethod("copyInputStream", java.io.InputStream.class, java.io.OutputStream.class);
        copyInputStream.setAccessible(true);
        copyInputStream.invoke(actions, new java.io.ByteArrayInputStream(new byte[0]), new java.io.ByteArrayOutputStream());

        Method buildDirectory = FileActions.class.getDeclaredMethod("buildDirectory", java.io.File.class);
        buildDirectory.setAccessible(true);
        Assert.assertFalse((Boolean) buildDirectory.invoke(actions, tempDirectory.resolve("built-by-reflection").toFile()));
        Path blockedParentForBuildDirectory = tempDirectory.resolve("blocked-parent-for-build-directory");
        Files.writeString(blockedParentForBuildDirectory, "blocks mkdirs", StandardCharsets.UTF_8);
        Assert.assertTrue((Boolean) buildDirectory.invoke(actions, blockedParentForBuildDirectory.resolve("child").toFile()));

        Method unpackArchiveFile = FileActions.class.getDeclaredMethod("unpackArchive", java.io.File.class, java.io.File.class);
        unpackArchiveFile.setAccessible(true);
        Assert.expectThrows(InvocationTargetException.class, () ->
                unpackArchiveFile.invoke(actions, tempDirectory.resolve("missing-archive.zip").toFile(), tempDirectory.toFile()));

        Method addFileToZip = FileActions.class.getDeclaredMethod("addFileToZip", String.class, String.class, java.util.zip.ZipOutputStream.class, boolean.class);
        addFileToZip.setAccessible(true);
        Assert.expectThrows(InvocationTargetException.class, () -> {
            try (var zipOutputStream = new java.util.zip.ZipOutputStream(new java.io.ByteArrayOutputStream())) {
                addFileToZip.invoke(actions, "missing", tempDirectory.resolve("missing-for-zip.txt").toString(), zipOutputStream, false);
            }
        });

        Method failWithData = FileActions.class.getDeclaredMethod("failAction", String.class, Exception[].class);
        failWithData.setAccessible(true);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> failWithData.invoke(actions, "forced failure", new Exception[]{new IOException("forced")}));
        Assert.assertTrue(thrown.getCause() instanceof RuntimeException);

        Method failWithThrowable = FileActions.class.getDeclaredMethod("failAction", Exception[].class);
        failWithThrowable.setAccessible(true);
        InvocationTargetException throwableFailure = Assert.expectThrows(InvocationTargetException.class,
                () -> failWithThrowable.invoke(actions, (Object) new Exception[]{new IOException("forced throwable")}));
        Assert.assertTrue(throwableFailure.getCause() instanceof RuntimeException);

        Method failWithActionName = FileActions.class.getDeclaredMethod("failAction", String.class, String.class, Exception[].class);
        failWithActionName.setAccessible(true);
        InvocationTargetException namedFailure = Assert.expectThrows(InvocationTargetException.class,
                () -> failWithActionName.invoke(actions, "customFailure", "forced data", new Exception[]{new IOException("forced named")}));
        Assert.assertTrue(namedFailure.getCause() instanceof RuntimeException);

        InvocationTargetException noCauseFailure = Assert.expectThrows(InvocationTargetException.class,
                () -> failWithData.invoke(actions, "forced data without cause", new Exception[0]));
        Assert.assertTrue(noCauseFailure.getCause().getMessage().contains("forced data without cause"));
    }

    @Test
    public void safeErrorBranchesShouldThrowForMissingOrInvalidInputs() throws IOException {
        FileActions actions = FileActions.getInstance(true);
        Path missing = tempDirectory.resolve("missing.txt");
        Path sourceFile = tempDirectory.resolve("file-instead-of-folder.txt");
        Files.writeString(sourceFile, "not-a-directory", StandardCharsets.UTF_8);

        Assert.expectThrows(RuntimeException.class, () -> actions.renameFile(missing.toString(), "renamed-missing.txt"));
        Assert.assertTrue(actions.getAbsolutePath("bad\0path").contains("bad"));
        Assert.assertTrue(actions.getAbsolutePath("bad\0", "path").contains("bad"));
        Assert.assertFalse(actions.doesFileExist("bad\0path"));
        Assert.assertFalse(actions.doesFileExist("bad\0", "path", 1));
        Assert.expectThrows(RuntimeException.class, () -> actions.listFilesInDirectory("bad\0path"));
        Assert.expectThrows(RuntimeException.class, () -> actions.listFilesInDirectory("bad\0path", (TrueFileFilter) null));
        Assert.expectThrows(RuntimeException.class, () -> actions.getFileList("bad\0path"));
        Assert.expectThrows(RuntimeException.class, () -> actions.readFile(missing.toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.readFileAsByteArray(missing.toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.copyFile(missing.toString(), tempDirectory.resolve("copy.txt").toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.copyFolder(missing.toString(), tempDirectory.resolve("copy-folder").toString()));
        Assert.assertEquals(actions.listFilesInDirectory(missing.toString()), "");
        Assert.assertTrue(actions.getFileList(missing.toString()).isEmpty());
        Assert.assertEquals(actions.listFilesInDirectory(sourceFile.toString()), "file-instead-of-folder.txt");
        Assert.assertEquals(actions.getFileList(sourceFile.toString()).size(), 1);
        Assert.expectThrows(RuntimeException.class, () -> actions.createFolder(sourceFile.toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.createFile(sourceFile.toString() + java.io.File.separator, "child.txt"));
        Assert.expectThrows(RuntimeException.class, () -> actions.writeToFile("bad\0path", "content"));
        Assert.expectThrows(RuntimeException.class, () -> actions.zipFiles(missing.toString(), tempDirectory.resolve("missing.zip").toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.unpackArchive(missing.toUri().toURL(), tempDirectory.resolve("unpack-missing").toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.downloadFile(missing.toUri().toURL().toString(), tempDirectory.resolve("download.txt").toString()));
        RuntimeException nullDownloadFailure = Assert.expectThrows(RuntimeException.class,
                () -> actions.downloadFile(null, tempDirectory.resolve("download.txt").toString()));
        Assert.assertTrue(nullDownloadFailure.getMessage().contains("Target File URL"));
        String missingJarResourceUrl = missing.toUri().toURL() + "!/assets/";
        Assert.expectThrows(RuntimeException.class, () -> actions.copyFolderFromJar(missingJarResourceUrl, tempDirectory.resolve("jar").toString()));
        Assert.expectThrows(RuntimeException.class, () -> actions.copyFileFromJar(missingJarResourceUrl, tempDirectory.resolve("jar").toString(), "one.txt"));
    }

    private Path createJarWithResources() throws IOException {
        Path jar = tempDirectory.resolve("resources.jar");
        try (JarOutputStream outputStream = new JarOutputStream(Files.newOutputStream(jar))) {
            outputStream.putNextEntry(new JarEntry("assets/"));
            outputStream.closeEntry();
            outputStream.putNextEntry(new JarEntry("assets/one.txt"));
            outputStream.write("one".getBytes(StandardCharsets.UTF_8));
            outputStream.closeEntry();
            outputStream.putNextEntry(new JarEntry("assets/nested/"));
            outputStream.closeEntry();
            outputStream.putNextEntry(new JarEntry("assets/nested/two.txt"));
            outputStream.write("two".getBytes(StandardCharsets.UTF_8));
            outputStream.closeEntry();
        }
        return jar;
    }

    private Path createZipWithTraversalEntry() throws IOException {
        Path zip = tempDirectory.resolve("traversal.zip");
        try (java.util.zip.ZipOutputStream outputStream = new java.util.zip.ZipOutputStream(Files.newOutputStream(zip))) {
            outputStream.putNextEntry(new ZipEntry("../evil.txt"));
            outputStream.write("evil".getBytes(StandardCharsets.UTF_8));
            outputStream.closeEntry();
        }
        return zip;
    }

    private Path createJarWithTraversalEntry() throws IOException {
        Path jar = tempDirectory.resolve("traversal.jar");
        try (JarOutputStream outputStream = new JarOutputStream(Files.newOutputStream(jar))) {
            outputStream.putNextEntry(new JarEntry("assets/"));
            outputStream.closeEntry();
            outputStream.putNextEntry(new JarEntry("assets/../evil.txt"));
            outputStream.write("evil".getBytes(StandardCharsets.UTF_8));
            outputStream.closeEntry();
        }
        return jar;
    }

    private boolean zipContains(Path zipFile, String entryNamePart) throws IOException {
        try (ZipInputStream zipInputStream = new ZipInputStream(Files.newInputStream(zipFile))) {
            ZipEntry entry;
            while ((entry = zipInputStream.getNextEntry()) != null) {
                if (entry.getName().contains(entryNamePart)) {
                    return true;
                }
            }
        }
        return false;
    }
}
