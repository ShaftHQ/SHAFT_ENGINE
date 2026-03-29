package testPackage.unitTests;

import com.shaft.tools.internal.support.AndroidApkBadgingReader;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

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
