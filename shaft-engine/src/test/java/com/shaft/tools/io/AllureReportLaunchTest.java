package com.shaft.tools.io;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.util.EnumSet;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

/**
 * JDK pin, classpath gate, and Windows {@code @argfile} for regenerating an Allure report (#7899).
 * Does not launch a GUI or a browser.
 */
public class AllureReportLaunchTest {

    @Test
    public void resolveJdkHomePrefersJavaHomeWhenItCanLoadTheEngine() {
        Path javaHome = Path.of("/jdks/temurin-25");
        Path older = Path.of("/jdks/temurin-21");
        Path resolved = AllureReportLaunch.resolveJdkHome(javaHome.toString(), List.of(older), 25,
                path -> path.equals(javaHome) ? 25 : 21);
        Assert.assertEquals(resolved, javaHome);
    }

    @Test
    public void resolveJdkHomeSkipsAJavaHomeOlderThanTheEngineClassFile() {
        Path javaHome = Path.of("/jdks/temurin-21");
        Path fallback = Path.of("/jdks/temurin-25");
        Path resolved = AllureReportLaunch.resolveJdkHome(javaHome.toString(), List.of(fallback), 25,
                path -> path.equals(fallback) ? 25 : 21);
        Assert.assertEquals(resolved, fallback);
    }

    @Test
    public void resolveJdkHomeFailsWhenNoJdkMeetsTheEngineClassFile() {
        IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                () -> AllureReportLaunch.resolveJdkHome("/jdks/temurin-21", List.of(Path.of("/jdks/other-21")), 25,
                        path -> 21));
        Assert.assertTrue(failure.getMessage().contains("25"));
    }

    @Test
    public void classFileMajor69RequiresJava25() {
        byte[] header = new byte[] {(byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE, 0, 0, 0, 69};
        Assert.assertEquals(AllureReportLaunch.javaMajorRequiredBy(header), 25);
    }

    @Test
    public void mavenOfflineIsUsedOnlyWhenShaftEngineIsAlreadyInTheLocalRepository() throws IOException {
        Path repository = Files.createTempDirectory("shaft-m2");
        List<String> online = AllureReportLaunch.mavenClasspathCommand(repository);
        Assert.assertFalse(online.contains("-o"), online.toString());
        Assert.assertTrue(online.contains("dependency:build-classpath"));

        Path jar = repository.resolve("io/github/shafthq/shaft-engine/10.4/shaft-engine-10.4.jar");
        Files.createDirectories(jar.getParent());
        Files.write(jar, new byte[] {0});
        List<String> offline = AllureReportLaunch.mavenClasspathCommand(repository);
        Assert.assertTrue(offline.contains("-o"), offline.toString());
    }

    @Test
    public void requireShaftClassFailsFastWhenTheClassIsAbsentAndPassesWhenPresent() throws IOException {
        Path root = Files.createTempDirectory("shaft-cp");
        Path emptyJar = jarWith(root.resolve("empty.jar"), null);
        IllegalStateException missing = Assert.expectThrows(IllegalStateException.class,
                () -> AllureReportLaunch.requireShaftClass(List.of(emptyJar)));
        Assert.assertTrue(missing.getMessage().contains("com/shaft/driver/SHAFT.class"), missing.getMessage());

        Path engineJar = jarWith(root.resolve("shaft.jar"), "com/shaft/driver/SHAFT.class");
        AllureReportLaunch.requireShaftClass(List.of(emptyJar, engineJar));
    }

    @Test
    public void classpathGoesThroughAJavaArgfileSoWindowsCanPassIt() throws IOException {
        Path root = Files.createTempDirectory("shaft-args");
        Path argFile = root.resolve("shaft-report.args");
        List<String> classpath = List.of("C:\\repo\\shaft engine.jar", "C:\\repo\\other.jar");
        AllureReportLaunch.writeClasspathArgFile(argFile, classpath, "ShaftReport.java");
        String text = Files.readString(argFile);
        Assert.assertTrue(text.startsWith("-cp"), text);
        Assert.assertTrue(text.contains("C:/repo/shaft engine.jar"), text);
        Assert.assertTrue(text.contains("ShaftReport.java"), text);

        List<String> command = AllureReportLaunch.javaCommand(argFile, "ShaftReport.java");
        Assert.assertEquals(command, List.of("java", "@" + argFile.toAbsolutePath().normalize(), "ShaftReport.java"));
        Assert.assertFalse(String.join(" ", command).contains("shaft engine.jar"));
    }

    @Test
    public void javacVersionParserReadsModernAndLegacyMajors() throws IOException {
        Assert.assertEquals(AllureReportLaunch.javaMajorFromJavacOutput("javac 25.0.1"), 25);
        Assert.assertEquals(AllureReportLaunch.javaMajorFromJavacOutput("javac 1.8.0_422"), 8);
        Path home = Files.createTempDirectory("fake-jdk");
        Path javac = home.resolve("bin/javac");
        Files.createDirectories(javac.getParent());
        Files.writeString(javac, "#!/bin/sh\nprintf '%s\\n' 'javac 25.0.1' 1>&2\n", StandardCharsets.UTF_8);
        Files.setPosixFilePermissions(javac, EnumSet.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_EXECUTE));
        Assert.assertEquals(AllureReportLaunch.jdkMajor(home), 25);
        Assert.assertTrue(AllureReportLaunch.engineJavaMajor() >= 21);
    }

    @Test
    public void shortHeaderAndUnrecognizedJavacAreRejected() {
        Assert.expectThrows(IllegalArgumentException.class,
                () -> AllureReportLaunch.javaMajorRequiredBy(new byte[] {0, 1, 2, 3, 4, 5, 6}));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> AllureReportLaunch.javaMajorFromJavacOutput(null));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> AllureReportLaunch.javaMajorFromJavacOutput("not javac"));
    }

    @Test
    public void jdkMajorOfNullIsUnknown() {
        Assert.assertEquals(AllureReportLaunch.jdkMajor(null), -1);
    }

    @Test
    public void blankJavaHomeSelectsTheFirstFallbackThatMeetsTheMajor() {
        Assert.expectThrows(IllegalArgumentException.class,
                () -> AllureReportLaunch.resolveJdkHome("/jdks/temurin-25", List.of(), 0, path -> 25));
        Path older = Path.of("/jdks/temurin-21");
        Path newer = Path.of("/jdks/temurin-25");
        Path resolved = AllureReportLaunch.resolveJdkHome(" ", List.of(older, newer), 25,
                path -> path.equals(newer) ? 25 : 21);
        Assert.assertEquals(resolved, newer);
    }

    @Test
    public void nullRepositoryAndSourcesJarDoNotUseOffline() throws IOException {
        List<String> online = AllureReportLaunch.mavenClasspathCommand(null);
        Assert.assertFalse(online.contains("-o"), online.toString());

        Path repository = Files.createTempDirectory("shaft-m2-sources");
        Path sources = repository.resolve("io/github/shafthq/shaft-engine/10.4/shaft-engine-10.4-sources.jar");
        Files.createDirectories(sources.getParent());
        Files.write(sources, new byte[] {0});
        List<String> command = AllureReportLaunch.mavenClasspathCommand(repository);
        Assert.assertFalse(command.contains("-o"), command.toString());
    }

    @Test
    public void requireShaftClassAcceptsADirectoryEntryAndRejectsNull() throws IOException {
        Assert.expectThrows(IllegalStateException.class, () -> AllureReportLaunch.requireShaftClass(null));
        Path root = Files.createTempDirectory("shaft-classes");
        Path classFile = root.resolve(AllureReportLaunch.SHAFT_CLASS_ENTRY);
        Files.createDirectories(classFile.getParent());
        Files.write(classFile, new byte[] {(byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE, 0, 0, 0, 69});
        AllureReportLaunch.requireShaftClass(List.of(root));
    }

    private static Path jarWith(Path jar, String entryName) throws IOException {
        Files.createDirectories(jar.getParent() == null ? Path.of(".") : jar.getParent());
        try (JarOutputStream output = new JarOutputStream(Files.newOutputStream(jar))) {
            if (entryName != null) {
                output.putNextEntry(new JarEntry(entryName));
                output.write(new byte[] {(byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE, 0, 0, 0, 69});
                output.closeEntry();
            }
        }
        return jar;
    }
}
