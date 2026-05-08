package testPackage.unitTests;

import com.shaft.tools.internal.support.AndroidApkBadgingReader;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Optional;
import java.util.UUID;

/**
 * Ensures APK badging parsing (used to infer {@code mobile_appPackage} / {@code mobile_appActivity}) is stable.
 */
public class AndroidApkBadgingReaderUnitTest {

    @Test(description = "Parse typical aapt dump badging lines for package and launchable activity")
    public void parseAaptDumpBadgingOutput_extractsPackageAndLaunchableActivity() throws IOException {
        String sample = """
                package: name='com.example.app' versionCode='42' versionName='1.0'
                sdkVersion:'30'
                launchable-activity: name='com.example.app.MainActivity'  label='' icon=''
                """;
        Optional<AndroidApkBadgingReader.PackageActivity> opt = AndroidApkBadgingReader.parseAaptDumpBadgingOutput(sample);
        Assert.assertTrue(opt.isPresent(), "Expected parsed package/activity");
        Assert.assertEquals(opt.get().packageName(), "com.example.app");
        Assert.assertEquals(opt.get().launchableActivity(), "com.example.app.MainActivity");
    }

    @Test(description = "Parse relative launchable-activity name from badging")
    public void parseAaptDumpBadgingOutput_relativeActivity() throws IOException {
        String sample = "package: name='com.foo'\nlaunchable-activity: name='.SplashActivity'  label=''\n";
        Optional<AndroidApkBadgingReader.PackageActivity> opt = AndroidApkBadgingReader.parseAaptDumpBadgingOutput(sample);
        Assert.assertTrue(opt.isPresent());
        Assert.assertEquals(opt.get().packageName(), "com.foo");
        Assert.assertEquals(opt.get().launchableActivity(), ".SplashActivity");
    }

    @Test(description = "Package line without launchable-activity still yields package")
    public void parseAaptDumpBadgingOutput_packageOnly() throws IOException {
        String sample = "package: name='com.onlypkg'\n";
        Optional<AndroidApkBadgingReader.PackageActivity> opt = AndroidApkBadgingReader.parseAaptDumpBadgingOutput(sample);
        Assert.assertTrue(opt.isPresent());
        Assert.assertEquals(opt.get().packageName(), "com.onlypkg");
        Assert.assertNull(opt.get().launchableActivity());
    }

    @Test(description = "Blank badging output yields empty optional")
    public void parseAaptDumpBadgingOutput_empty() throws IOException {
        Assert.assertTrue(AndroidApkBadgingReader.parseAaptDumpBadgingOutput("").isEmpty());
        Assert.assertTrue(AndroidApkBadgingReader.parseAaptDumpBadgingOutput(null).isEmpty());
    }

    @Test(description = "No package line should return empty optional")
    public void parseAaptDumpBadgingOutput_withoutPackage_returnsEmpty() throws IOException {
        String sample = "launchable-activity: name='com.example.MainActivity'  label='' icon=''\n";
        Assert.assertTrue(AndroidApkBadgingReader.parseAaptDumpBadgingOutput(sample).isEmpty());
    }

    @Test(description = "Invalid APK input should return empty without reading Android SDK")
    public void readPackageAndLaunchableActivity_invalidApkInput_returnsEmpty() {
        Assert.assertTrue(AndroidApkBadgingReader.readPackageAndLaunchableActivity(null).isEmpty());
        Assert.assertTrue(AndroidApkBadgingReader.readPackageAndLaunchableActivity(new File("not-existing.apk")).isEmpty());
        Assert.assertTrue(AndroidApkBadgingReader.readPackageAndLaunchableActivity(new File("pom.xml")).isEmpty());
    }

    @Test(description = "Read method should handle non-APK content gracefully and return empty")
    public void readPackageAndLaunchableActivity_nonApkContent_returnsEmpty() throws IOException {
        Path fakeApk = Files.createTempFile("fake-apk-", ".apk");
        try {
            Files.writeString(fakeApk, "not a real apk");
            Optional<AndroidApkBadgingReader.PackageActivity> result =
                    AndroidApkBadgingReader.readPackageAndLaunchableActivity(fakeApk.toFile());
            Assert.assertTrue(result.isEmpty(), "Invalid APK content should not produce package/activity metadata.");
        } finally {
            Files.deleteIfExists(fakeApk);
        }
    }

    @Test(description = "resolveAaptExecutable should prefer aapt2 then aapt and return null when absent")
    public void resolveAaptExecutable_selectionOrder() throws Exception {
        Method method = AndroidApkBadgingReader.class.getDeclaredMethod("resolveAaptExecutable", File.class);
        method.setAccessible(true);

        Path dir = Files.createTempDirectory("build-tools-dir");
        String osName = System.getProperty("os.name", "").toLowerCase();
        String aapt2Name = osName.contains("win") ? "aapt2.exe" : "aapt2";
        String aaptName = osName.contains("win") ? "aapt.exe" : "aapt";

        Assert.assertNull(method.invoke(null, dir.toFile()));

        Path aapt = Files.createFile(dir.resolve(aaptName));
        Assert.assertEquals(method.invoke(null, dir.toFile()), aapt.toFile().getAbsolutePath());

        Path aapt2 = Files.createFile(dir.resolve(aapt2Name));
        Assert.assertEquals(method.invoke(null, dir.toFile()), aapt2.toFile().getAbsolutePath());

        Files.deleteIfExists(aapt2);
        Files.deleteIfExists(aapt);
        Files.deleteIfExists(dir);
    }

    @Test(description = "firstNonBlank should return the first non-blank value or null")
    public void firstNonBlank_returnsExpectedValue() throws Exception {
        Method method = AndroidApkBadgingReader.class.getDeclaredMethod("firstNonBlank", String.class, String.class);
        method.setAccessible(true);

        Assert.assertEquals(method.invoke(null, "a", "b"), "a");
        Assert.assertEquals(method.invoke(null, " ", "b"), "b");
        Assert.assertNull(method.invoke(null, "", " "));
    }

    @Test(description = "Reader should use newest build-tools entries, skip missing aapt, and parse valid output")
    public void readPackageAndLaunchableActivity_prefersNewestBuildToolsAndParsesOutput() throws IOException {
        String androidHome = resolveAndroidHomeOrSkip();

        Path buildTools = Path.of(androidHome, "build-tools");
        Path successDir = buildTools.resolve("test_success_" + UUID.randomUUID());
        Path noExecDir = buildTools.resolve("test_noexec_" + UUID.randomUUID());
        Path fakeApk = Files.createTempFile("fake-readable-", ".apk");
        String executableName = System.getProperty("os.name", "").toLowerCase().contains("win") ? "aapt2.exe" : "aapt2";

        try {
            Files.createDirectories(successDir);
            Files.createDirectories(noExecDir);

            // Force order: noExec first (continue), then success (parse and return).
            Files.setLastModifiedTime(successDir, FileTime.fromMillis(System.currentTimeMillis() - 5000));
            Files.setLastModifiedTime(noExecDir, FileTime.fromMillis(System.currentTimeMillis()));

            Path fakeAapt2 = successDir.resolve(executableName);
            String script = """
                    #!/bin/sh
                    echo "package: name='com.coverage.reader'"
                    echo "launchable-activity: name='com.coverage.reader.MainActivity'"
                    exit 0
                    """;
            Files.writeString(fakeAapt2, script);
            fakeAapt2.toFile().setExecutable(true);

            Files.writeString(fakeApk, "placeholder");
            Optional<AndroidApkBadgingReader.PackageActivity> result =
                    AndroidApkBadgingReader.readPackageAndLaunchableActivity(fakeApk.toFile());

            Assert.assertTrue(result.isPresent());
            Assert.assertEquals(result.get().packageName(), "com.coverage.reader");
            Assert.assertEquals(result.get().launchableActivity(), "com.coverage.reader.MainActivity");
        } finally {
            deleteIfExists(successDir.resolve(executableName));
            deleteIfExists(successDir);
            deleteIfExists(noExecDir);
            deleteIfExists(fakeApk);
        }
    }

    @Test(description = "Reader should handle command start failures and continue scanning build-tools versions")
    public void readPackageAndLaunchableActivity_handlesAaptStartFailure() throws IOException {
        String androidHome = resolveAndroidHomeOrSkip();

        Path buildTools = Path.of(androidHome, "build-tools");
        Path failingDir = buildTools.resolve("test_failing_" + UUID.randomUUID());
        Path fakeApk = Files.createTempFile("fake-failing-", ".apk");
        String executableName = System.getProperty("os.name", "").toLowerCase().contains("win") ? "aapt2.exe" : "aapt2";
        try {
            Files.createDirectories(failingDir);
            Path failingExecutable = failingDir.resolve(executableName);
            Files.writeString(failingExecutable, "#!/bin/sh\necho should-not-run\n");
            failingExecutable.toFile().setExecutable(false);
            Files.setLastModifiedTime(failingDir, FileTime.fromMillis(System.currentTimeMillis() + 5000));

            Files.writeString(fakeApk, "placeholder");
            Assert.assertTrue(AndroidApkBadgingReader.readPackageAndLaunchableActivity(fakeApk.toFile()).isEmpty());
        } finally {
            deleteIfExists(failingDir.resolve(executableName));
            deleteIfExists(failingDir);
            deleteIfExists(fakeApk);
        }
    }

    private static String resolveAndroidHomeOrSkip() {
        String androidHome = System.getenv("ANDROID_HOME");
        if (androidHome == null || androidHome.isBlank()) {
            androidHome = System.getenv("ANDROID_SDK_ROOT");
        }
        if (androidHome == null || androidHome.isBlank()) {
            throw new SkipException("ANDROID_HOME or ANDROID_SDK_ROOT must be available.");
        }
        return androidHome;
    }

    private static void deleteIfExists(Path path) throws IOException {
        Files.deleteIfExists(path);
    }

    @Test(description = "Optional end-to-end: read real APK when SHAFT_TEST_APK and ANDROID_HOME are set")
    public void readPackageFromRealApk_whenEnvPresent() {
        String apkPath = System.getenv("SHAFT_TEST_APK");
        String androidHome = System.getenv("ANDROID_HOME");
        if (androidHome == null || androidHome.isBlank()) {
            androidHome = System.getenv("ANDROID_SDK_ROOT");
        }
        if (apkPath == null || apkPath.isBlank() || androidHome == null || androidHome.isBlank()) {
            throw new SkipException("Set SHAFT_TEST_APK and ANDROID_HOME (or ANDROID_SDK_ROOT) to run this check.");
        }
        File apk = new File(apkPath);
        if (!apk.isFile()) {
            throw new SkipException("SHAFT_TEST_APK is not a file: " + apkPath);
        }
        Optional<AndroidApkBadgingReader.PackageActivity> opt = AndroidApkBadgingReader.readPackageAndLaunchableActivity(apk);
        Assert.assertTrue(opt.isPresent(), "aapt should return package for a valid APK with SDK on PATH");
        Assert.assertNotNull(opt.get().packageName());
        Assert.assertFalse(opt.get().packageName().isBlank());
    }
}
