package com.shaft.tools.io;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
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
    public void javacVersionParserReadsModernAndLegacyMajors() throws Exception {
        Assert.assertEquals(AllureReportLaunch.javaMajorFromJavacOutput("javac 25.0.1"), 25);
        Assert.assertEquals(AllureReportLaunch.javaMajorFromJavacOutput("javac 1.8.0_422"), 8);
        Path home = Files.createTempDirectory("fake-jdk");
        if (windowsOs()) {
            Path shell = home.resolve("bin/javac");
            Files.createDirectories(shell.getParent());
            Files.writeString(shell, "#!/bin/sh\nprintf '%s\\n' 'javac 25.0.1' 1>&2\n", StandardCharsets.UTF_8);
            shell.toFile().setExecutable(true);
            Assert.assertEquals(AllureReportLaunch.jdkMajor(home), -1);
        }
        writeFakeJavac(home, "javac 25.0.1", true);
        Assert.assertEquals(AllureReportLaunch.jdkMajor(home), 25);
        Assert.assertTrue(AllureReportLaunch.engineJavaMajor() >= 21);
    }

    @Test
    public void javacVersionCommandUsesTheAbsoluteExecutable() throws IOException {
        Path home = Files.createTempDirectory("fake-jdk-cmd");
        String previous = System.getProperty("os.name");
        try {
            System.setProperty("os.name", "Windows 11");
            List<String> windows = AllureReportLaunch.javacVersionCommand(home);
            Assert.assertEquals(windows.get(0),
                    home.resolve("bin/javac.exe").toAbsolutePath().normalize().toString());
            Assert.assertNotEquals(windows.get(0), "bin\\javac.exe");
            Assert.assertEquals(windows.get(1), "-version");

            System.setProperty("os.name", "Linux");
            List<String> unix = AllureReportLaunch.javacVersionCommand(home);
            Assert.assertEquals(unix.get(0),
                    home.resolve("bin/javac").toAbsolutePath().normalize().toString());
            Assert.assertNotEquals(unix.get(0), "bin/javac");
            Assert.assertEquals(unix.get(1), "-version");
        } finally {
            System.setProperty("os.name", previous);
        }
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

    @Test
    public void privateConstructorAndNullClassHeaderAreRejected() throws Exception {
        Constructor<AllureReportLaunch> constructor = AllureReportLaunch.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
        Assert.expectThrows(IllegalArgumentException.class, () -> AllureReportLaunch.javaMajorRequiredBy(null));
    }

    @Test
    public void javacWithoutAMinorOrAUsableBinaryStaysUnknown() throws Exception {
        Assert.assertEquals(AllureReportLaunch.javaMajorFromJavacOutput("javac 1"), 1);

        Path home = Files.createTempDirectory("fake-jdk-gaps");
        Assert.assertEquals(AllureReportLaunch.jdkMajor(home), -1);

        writeFakeJavac(home, "not a version", false);
        Assert.assertEquals(AllureReportLaunch.jdkMajor(home), -1);

        Path javac = home.resolve(windowsOs() ? "bin/javac.exe" : "bin/javac");
        Assert.assertTrue(javac.toFile().setExecutable(true) || windowsOs(), javac.toString());
        Assert.assertEquals(AllureReportLaunch.jdkMajor(home), -1);
    }

    @Test
    public void anInterruptedJdkProbeReturnsUnknown() throws Exception {
        Path home = Files.createTempDirectory("fake-jdk-interrupt");
        // Exit immediately. A still-running child makes waitFor throw, which already returned -1
        // and hid the clean-exit hole.
        writeFakeJavac(home, "javac 25.0.1", true);
        Thread.currentThread().interrupt();
        try {
            Assert.assertEquals(AllureReportLaunch.jdkMajor(home), -1);
            Assert.assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
    }

    @Test
    public void anInterruptedWaitForReturnsUnknown() throws Exception {
        Path home = Files.createTempDirectory("fake-jdk-wait");
        Path started = home.resolve("started");
        Path childPid = home.resolve("child.pid");
        writeBlockingFakeJavac(home, started, childPid);
        int[] major = {Integer.MIN_VALUE};
        boolean[] restored = {false};
        Thread caller = new Thread(() -> {
            major[0] = AllureReportLaunch.jdkMajor(home);
            restored[0] = Thread.currentThread().isInterrupted();
        });
        caller.setDaemon(true);
        caller.start();
        try {
            long deadline = System.nanoTime() + java.time.Duration.ofSeconds(5).toNanos();
            while (Files.notExists(started) || Files.notExists(childPid)) {
                Assert.assertTrue(caller.isAlive(), "probe ended before the sleeper signaled");
                Assert.assertTrue(System.nanoTime() < deadline, "sleeper did not signal");
                Thread.sleep(5);
            }
            long child = Long.parseLong(Files.readString(childPid).trim());
            Assert.assertTrue(ProcessHandle.of(child).map(ProcessHandle::isAlive).orElse(false),
                    "sleeper was not alive when interrupted");
            caller.interrupt();
            caller.join(java.time.Duration.ofSeconds(5).toMillis());
            Assert.assertFalse(caller.isAlive(), "probe did not finish after interrupt");
            Assert.assertEquals(major[0], -1);
            Assert.assertTrue(restored[0]);
            Assert.assertFalse(childStillRunning(child), "child left running after interrupt");
        } finally {
            caller.interrupt();
            caller.join(java.time.Duration.ofSeconds(2).toMillis());
            destroyRecordedChild(childPid);
        }
    }

    @Test
    public void resolveJdkHomeIgnoresNullHomeNullFallbacksAndNullCandidates() {
        Path preferred = Path.of("/jdks/temurin-25");
        Assert.assertEquals(AllureReportLaunch.resolveJdkHome(preferred.toString(), null, 25, path -> 25), preferred);

        Path fallback = Path.of("/jdks/temurin-25");
        Path resolved = AllureReportLaunch.resolveJdkHome(null, Arrays.asList(null, fallback), 25,
                path -> fallback.equals(path) ? 25 : 1);
        Assert.assertEquals(resolved, fallback);

        IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                () -> AllureReportLaunch.resolveJdkHome(null, null, 25, path -> 1));
        Assert.assertTrue(failure.getMessage().contains("25"), failure.getMessage());
    }

    @Test
    public void rootPathIsNotAnEngineJar() throws Exception {
        Method engineJar = AllureReportLaunch.class.getDeclaredMethod("isEngineJar", Path.class);
        engineJar.setAccessible(true);
        Assert.assertFalse((Boolean) engineJar.invoke(null, Path.of("/")));

        Method shaftClass = AllureReportLaunch.class.getDeclaredMethod("containsShaftClass", Path.class);
        shaftClass.setAccessible(true);
        Assert.assertFalse((Boolean) shaftClass.invoke(null, Path.of("/")));
    }

    @Test
    public void javadocAndNonJarFilesDoNotEnableOffline() throws IOException {
        Path repository = Files.createTempDirectory("shaft-m2-javadoc");
        Path engine = repository.resolve("io/github/shafthq/shaft-engine/10.4");
        Files.createDirectories(engine);
        Files.write(engine.resolve("shaft-engine-10.4-javadoc.jar"), new byte[] {0});
        Files.writeString(engine.resolve("notes.txt"), "not an engine jar");
        Files.writeString(engine.resolve("shaft-engine-10.4.txt"), "not a jar");
        List<String> command = AllureReportLaunch.mavenClasspathCommand(repository);
        Assert.assertFalse(command.contains("-o"), command.toString());

        Path fileAsEngine = Files.createTempDirectory("shaft-m2-file");
        Path engineFile = fileAsEngine.resolve("io/github/shafthq/shaft-engine");
        Files.createDirectories(engineFile.getParent());
        Files.writeString(engineFile, "not a directory");
        List<String> online = AllureReportLaunch.mavenClasspathCommand(fileAsEngine);
        Assert.assertFalse(online.contains("-o"), online.toString());
    }

    @Test
    public void requireShaftClassRejectsMissingPlainEmptyAndCorruptEntries() throws IOException {
        Path root = Files.createTempDirectory("shaft-cp-gaps");
        Path plain = root.resolve("notes.txt");
        Files.writeString(plain, "not a jar");
        Path emptyDir = Files.createDirectories(root.resolve("classes"));
        Path corrupt = root.resolve("corrupt.jar");
        Files.writeString(corrupt, "this is not a zip");
        IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                () -> AllureReportLaunch.requireShaftClass(Arrays.asList(
                        null, root.resolve("missing.jar"), plain, emptyDir, corrupt)));
        Assert.assertTrue(failure.getMessage().contains(AllureReportLaunch.SHAFT_CLASS_ENTRY), failure.getMessage());
    }

    @Test
    public void argfileSkipsBlankTrailingAndQuotesOnlyWhenNeeded() throws IOException {
        Path root = Files.createTempDirectory("shaft-args-plain");
        Path plain = root.resolve("plain.args");
        AllureReportLaunch.writeClasspathArgFile(plain, List.of("a.jar", "b.jar"), " ");
        String plainText = Files.readString(plain);
        Assert.assertTrue(plainText.contains("a.jar" + java.io.File.pathSeparator + "b.jar"), plainText);
        Assert.assertFalse(plainText.contains("\""), plainText);
        Assert.assertEquals(plainText.lines().count(), 2);

        Path tabbed = root.resolve("tabbed.args");
        AllureReportLaunch.writeClasspathArgFile(tabbed, List.of("a\tb.jar"), null);
        Assert.assertTrue(Files.readString(tabbed).contains("\""), Files.readString(tabbed));

        Path quoted = root.resolve("quoted.args");
        AllureReportLaunch.writeClasspathArgFile(quoted, List.of("jar \"name\".jar"), "");
        Assert.assertTrue(Files.readString(quoted).contains("\\\""), Files.readString(quoted));

        Path bare = Path.of("shaft-report-launch-coverage.args");
        Assert.assertNull(bare.getParent());
        try {
            AllureReportLaunch.writeClasspathArgFile(bare, List.of(), null);
            Assert.assertTrue(Files.readString(bare).startsWith("-cp"), Files.readString(bare));
        } finally {
            Files.deleteIfExists(bare);
        }

        Assert.assertEquals(AllureReportLaunch.javaCommand(plain, (String[]) null),
                List.of("java", "@" + plain.toAbsolutePath().normalize()));
    }

    private static boolean windowsOs() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
    }

    /**
     * Windows CreateProcess does not run a batch file stored as {@code javac.exe}.
     * The live probe there is a console image with that name; other systems use a shell script.
     */
    private static void writeBlockingFakeJavac(Path home, Path started, Path childPid)
            throws IOException, InterruptedException {
        Path javac = home.resolve(windowsOs() ? "bin/javac.exe" : "bin/javac");
        Files.createDirectories(javac.getParent());
        if (windowsOs()) {
            writeWindowsConsoleExe(javac, "javac 25.0.1", started, childPid);
        } else {
            // Keep the pipe open. Closing it lets readAllBytes return, which hides the Windows
            // failure where the console close does not deliver EOF and the read never throws.
            String signal = started.toAbsolutePath().toString().replace("'", "'\\''");
            String pid = childPid.toAbsolutePath().toString().replace("'", "'\\''");
            Files.writeString(javac, """
                    #!/bin/sh
                    printf '%%s\\n' 'javac 25.0.1' >&2
                    printf '%%s\\n' "$$" > '%s'
                    touch '%s'
                    sleep 30
                    """.formatted(pid, signal), StandardCharsets.UTF_8);
        }
        Assert.assertTrue(javac.toFile().setExecutable(true) || windowsOs(), javac.toString());
    }

    private static boolean childStillRunning(long child) throws InterruptedException {
        long deadline = System.nanoTime() + java.time.Duration.ofSeconds(2).toNanos();
        boolean alive = true;
        while (System.nanoTime() < deadline) {
            alive = ProcessHandle.of(child).map(ProcessHandle::isAlive).orElse(false);
            if (!alive) {
                return false;
            }
            Thread.sleep(20);
        }
        return alive;
    }

    private static void destroyRecordedChild(Path childPid) {
        if (Files.notExists(childPid)) {
            return;
        }
        try {
            long child = Long.parseLong(Files.readString(childPid).trim());
            ProcessHandle.of(child).ifPresent(ProcessHandle::destroyForcibly);
        } catch (IOException | NumberFormatException exception) {
            // Best-effort cleanup so a failed probe does not leave the sleeper behind.
        }
    }

    private static void writeFakeJavac(Path home, String versionLine, boolean executable)
            throws IOException, InterruptedException {
        Path javac = home.resolve(windowsOs() ? "bin/javac.exe" : "bin/javac");
        Files.createDirectories(javac.getParent());
        if (windowsOs()) {
            writeWindowsConsoleExe(javac, versionLine, null, null);
        } else {
            Files.writeString(javac, "#!/bin/sh\nprintf '%s\\n' '" + versionLine + "' 1>&2\n",
                    StandardCharsets.UTF_8);
        }
        boolean marked = javac.toFile().setExecutable(executable);
        if (!windowsOs()) {
            Assert.assertTrue(marked, javac.toString());
        }
    }

    private static void writeWindowsConsoleExe(Path exe, String versionLine, Path started, Path childPid)
            throws IOException, InterruptedException {
        Path source = exe.resolveSibling("FakeJavac.cs");
        String afterWrite = "";
        if (started != null) {
            String signal = started.toAbsolutePath().toString().replace("\\", "\\\\").replace("\"", "\\\"");
            String pidStatement = "";
            if (childPid != null) {
                String pid = childPid.toAbsolutePath().toString().replace("\\", "\\\\").replace("\"", "\\\"");
                pidStatement = "System.IO.File.WriteAllText(\"" + pid
                        + "\", System.Diagnostics.Process.GetCurrentProcess().Id.ToString());";
            }
            afterWrite = """
                        System.Console.Out.Flush();
                        System.Console.Error.Flush();
                        System.Console.OpenStandardOutput().Close();
                        System.Console.OpenStandardError().Close();
                        %s
                        System.IO.File.WriteAllText("%s", "started");
                        System.Threading.Thread.Sleep(30000);
                    """.formatted(pidStatement, signal);
        }
        String program = """
                public class FakeJavac {
                    public static void Main() {
                        System.Console.Error.WriteLine("%s");
                %s    }
                }
                """.formatted(versionLine.replace("\\", "\\\\").replace("\"", "\\\""), afterWrite);
        Files.writeString(source, program, StandardCharsets.UTF_8);
        String windir = System.getenv("WINDIR");
        Path root = Path.of(windir == null || windir.isBlank() ? "C:\\Windows" : windir);
        Path csc = root.resolve("Microsoft.NET").resolve("Framework64").resolve("v4.0.30319").resolve("csc.exe");
        if (!Files.isRegularFile(csc)) {
            csc = root.resolve("Microsoft.NET").resolve("Framework").resolve("v4.0.30319").resolve("csc.exe");
        }
        if (!Files.isRegularFile(csc)) {
            throw new IOException("csc.exe was not found to build " + exe);
        }
        Process process = new ProcessBuilder(csc.toString(), "/nologo", "/t:exe",
                "/out:" + exe.toAbsolutePath(), source.toAbsolutePath().toString())
                .redirectErrorStream(true)
                .start();
        String log = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (process.waitFor() != 0 || !Files.isRegularFile(exe)) {
            throw new IOException("csc failed for " + exe + ": " + log);
        }
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
