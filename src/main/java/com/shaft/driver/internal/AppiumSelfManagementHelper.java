package com.shaft.driver.internal;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportHelper;
import io.qameta.allure.Step;
import lombok.Getter;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;

public class AppiumSelfManagementHelper {
    // TODO: implement new environment variables for dockerized instance (or remove the code)
    private static AppiumSelfManagementHelper singleInstance = null;
    @SuppressWarnings("FieldCanBeLocal")
    private final String cliToolsVersion = "9477386_latest";
    @SuppressWarnings("FieldCanBeLocal")
    private final String nodeJsVersion = "v18.14.0";
    @Getter
    private static final boolean terminateAppiumContainersAfterExecution = false;

    private final String androidSelfManagedEnvironmentLocation = System.getProperty("user.home") + File.separator + ".shaft" + File.separator + "android" + File.separator;
    private final String subPathToBin = "cmdline-tools" + File.separator + "latest" + File.separator + "bin";
    @SuppressWarnings("FieldCanBeLocal")
    private final String subPathToPlatform = "platform-tools";

    private AppiumSelfManagementHelper() {
        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
        // TODO: check if user has admin access
        // TODO: create a list to manage URLs for other OSs

        //check that they are installed and can be executed
        ReportManager.logDiscrete("Verifying Appium self-managed execution environment...");

        // FORK to SET UP ANDROID SDK CLI AND SETUP APPIUM SERVER IN PARALLEL
//        ScheduledExecutorService prepareAppiumSelfManagedEnvironment = Executors.newScheduledThreadPool(2);
//        if (Properties.platform.targetOperatingSystem().equals(OperatingSystems.ANDROID)) {
//            prepareAppiumSelfManagedEnvironment.execute(() -> {
//                try {
        setupAndroidSDKCLITools();
//                } catch (InterruptedException e) {
//                    throw new RuntimeException(e);
//                }
        setupAndroidPackages();
        launchAndroidEmulator();
//                prepareAppiumSelfManagedEnvironment.shutdown();
//            });
//        }

//        prepareAppiumSelfManagedEnvironment.execute(() -> {
        setupNPM();
        setupAppiumComponents();
        launchAppiumServer();
//            prepareAppiumSelfManagedEnvironment.shutdown();
//        });

    }

    @Step("Setting up Appium self-managed execution environment")
    public static void setupAppiumSelfManagedExecutionPrerequisites() {
        if (singleInstance == null) {
            singleInstance = new AppiumSelfManagementHelper();
        }
    }

    @Step("Setting up Android SDK CLI tools")
    private void setupAndroidSDKCLITools() {
        ReportManager.logDiscrete("Verifying Android SDK CLI tools...");
        // using an already existing android_home will cause issues upgrading due to lack of admin access.
//        String ANDROID_HOME = System.getenv("ANDROID_HOME");
//        System.setProperty("ANDROID_HOME", ANDROID_HOME);

        // setting a static and self-managed android home directory is the better approach to fully control all downloaded binaries
        System.setProperty("ANDROID_HOME", androidSelfManagedEnvironmentLocation);

        if (!FileActions.getInstance().doesFileExist(System.getProperty("ANDROID_HOME") + subPathToBin)) {
            String url = "https://dl.google.com/android/repository/commandlinetools-${OS}-" + cliToolsVersion + ".zip";
            if (SystemUtils.IS_OS_MAC) {
                url = url.replace("${OS}", "mac");
            } else if (SystemUtils.IS_OS_WINDOWS) {
                url = url.replace("${OS}", "win");
            } else {
                //default is linux
                url = url.replace("${OS}", "linux");
            }
            ReportManager.logDiscrete("Downloading and installing Android SDK CLI Tools from " + url);
            ReportHelper.disableLogging();
            try {
                FileActions.getInstance().unpackArchive(URI.create(url).toURL(), androidSelfManagedEnvironmentLocation);
                FileActions.getInstance().copyFolder(androidSelfManagedEnvironmentLocation + "cmdline-tools" + File.separator + "bin", androidSelfManagedEnvironmentLocation + subPathToBin);
                ReportManager.logDiscrete("Successfully prepared Android SDK CLI tools.");
            } catch (Throwable throwable) {
                ReportHelper.enableLogging();
                FailureReporter.fail(AppiumSelfManagementHelper.class, "Failed to prepare Android SDK CLI tools.", throwable);
            }
        }

        ReportManager.logDiscrete("Found Android SDK CLI Tools at " + System.getProperty("ANDROID_HOME") + subPathToBin);
    }

    @Step("Setting up Android packages")
    private void setupAndroidPackages() {
        ReportManager.logDiscrete("Verifying Android packages...");
        String ANDROID_HOME = System.getProperty("ANDROID_HOME");
        ReportHelper.disableLogging();

        //https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_environment_variables?view=powershell-7.3#saving-environment-variables-with-setenvironmentvariable
        var path = TerminalActions.getInstance(false, false).performTerminalCommand("[Environment]::GetEnvironmentVariable('Path', 'User')");

        // add only the missing properties as user environment variables
        if (!path.contains(ANDROID_HOME + "cmdline-tools" + File.separator + "latest;"))
            path = ANDROID_HOME + "cmdline-tools" + File.separator + "latest;" + path;
        if (!path.contains(ANDROID_HOME + subPathToBin + ";")) path = ANDROID_HOME + subPathToBin + ";" + path;
        if (!path.contains(ANDROID_HOME + subPathToPlatform + ";"))
            path = ANDROID_HOME + subPathToPlatform + ";" + path;
        if (!path.contains(ANDROID_HOME + "bin;")) path = ANDROID_HOME + "bin;" + path;
        if (!path.contains(ANDROID_HOME + "bin;")) path = ANDROID_HOME + "lib;" + path;

        // if nothing is augmented, then this would only reset the path variable without messing it up
        LinkedList<String> setEnvironmentVariables = new LinkedList<>(Arrays.asList(
                "[Environment]::SetEnvironmentVariable('JAVA_HOME', '" + System.getProperty("java.home") + "', 'User')",
                "[Environment]::SetEnvironmentVariable('ANDROID_HOME', '" + ANDROID_HOME + "', 'User')",
                "[Environment]::SetEnvironmentVariable('Path', '" + path + "', 'User')"));
        TerminalActions.getInstance(false, true).performTerminalCommands(setEnvironmentVariables);

        String avdList = TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')",
                "avdmanager list avd"));
        ReportHelper.enableLogging();

        var targetAndroidVersion = Properties.mobile.selfManagedAndroidSDKVersion();
        boolean doesAVDExist = avdList.contains(".avd") && avdList.contains(getAvdName()) && avdList.contains("Based on: Android " + targetAndroidVersion);
        if (!doesAVDExist) {
            var packages = Arrays.asList("'emulator'", "'patcher;v4'", "'platform-tools'",
                    "'build-tools;" + targetAndroidVersion + ".0.0'", "'platforms;android-" + targetAndroidVersion + "'", "'skiaparser;3'", "'sources;android-" + targetAndroidVersion + "'"
                    , "'system-images;android-" + targetAndroidVersion + ";default;x86_64'");
            ReportManager.logDiscrete("Downloading and updating required android packages " + String.join(", ", packages) + "...");
            try {
                // update sdkmanager
                TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')",
                        "echo \"y\" | sdkmanager --update"));
                // downloading packages
                LinkedList<String> commands = new LinkedList<>();
                commands.add("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')");
                packages.forEach(packageName -> commands.add("echo \"y\" | sdkmanager --install " + packageName));
                TerminalActions.getInstance(false, true).performTerminalCommands(commands);
                // accepting licenses
                TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')",
                        "echo \"y\" | sdkmanager --licenses"));
                // creating avd
                TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')",
                        "avdmanager  create avd -n " + getAvdName() + " -d pixel --package " + packages.get(packages.size() - 1)));
                ReportManager.logDiscrete("Successfully prepared android packages.");
            } catch (Throwable throwable) {
                FailureReporter.fail(AppiumSelfManagementHelper.class, "Failed to prepare android packages.", throwable);
            }
        }
    }

    private String getAvdName() {
        return "Pixel_android" + Properties.mobile.selfManagedAndroidSDKVersion() + "_x86_64";
    }

    @Step("Launching Android emulator")
    private void launchAndroidEmulator() {
        // TODO: Handle Emulator is already up and running
        String ANDROID_HOME = System.getProperty("ANDROID_HOME");
        String avdName = getAvdName();
        ReportManager.logDiscrete("Launching Android emulator '" + avdName + "'...");

        try {
            TerminalActions.getInstance(true, false).performTerminalCommands(Arrays.asList("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')",
                    "cd " + ANDROID_HOME + "/emulator/",
                    ".\\emulator -avd " + avdName + " -no-snapshot-load  -gpu host -no-audio -no-boot-anim -camera-back none -camera-front none -qemu -m 2048"));

            ReportManager.logDiscrete("Successfully launched emulator.");
        } catch (Throwable throwable) {
            FailureReporter.fail(AppiumSelfManagementHelper.class, "Failed to launch emulator.", throwable);
        }
    }

    @Step("Setting up Node.js")
    private void setupNPM() {
        ReportManager.logDiscrete("Verifying NPM installation...");

        ReportHelper.disableLogging();
        String NPM_VERSION = TerminalActions.getInstance(false, false).performTerminalCommand("npm --version").trim();
        ReportHelper.enableLogging();
        boolean isNPMInstalled = NPM_VERSION.matches(".*[0-9].*");
        if (!isNPMInstalled) {
            // install npm
            String url = "https://nodejs.org/dist/" + nodeJsVersion + "/node-" + nodeJsVersion + "-";
            if (SystemUtils.IS_OS_MAC) {
                url = url + "darwin-x64.tar.gz";
            } else if (SystemUtils.IS_OS_WINDOWS) {
                url = url + "win-x64.zip";
            } else {
                //default is linux
                url = url + "linux-x64.tar.xzp";
            }
            ReportManager.logDiscrete("Downloading and installing Node.js from " + url);
            try {
                FileActions.getInstance().unpackArchive(URI.create(url).toURL(), androidSelfManagedEnvironmentLocation + "npm" + File.separator);
                ReportManager.logDiscrete("Successfully prepared Node.js.");
            } catch (Throwable throwable) {
                FailureReporter.fail(AppiumSelfManagementHelper.class, "Failed to prepare Node.js.", throwable);
            }
        }
    }

    @Step("Setting up Appium server")
    private void setupAppiumComponents() {
        ReportManager.logDiscrete("Verifying Appium 2.x server and components...");

        ReportHelper.disableLogging();
        String APPIUM_VERSION = TerminalActions.getInstance(false, false).performTerminalCommand("appium --version");
        ReportHelper.enableLogging();

        APPIUM_VERSION = APPIUM_VERSION.contains("info") ? APPIUM_VERSION.split("info")[0].trim() : APPIUM_VERSION.trim();

        var isFirstInstall = false;
        if (APPIUM_VERSION.contains("The term 'appium' is not recognized") || !APPIUM_VERSION.startsWith("2.")) {
            //not installed or not up to date
            ReportManager.logDiscrete("Removing any instance of the deprecated Appium 1.x, and installing the latest Appium 2.x server...");

            TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList(
                    "cd " + androidSelfManagedEnvironmentLocation + File.separator + "npm",
                    "npm uninstall appium -g", "npm uninstall appium",
                    "npm install --global --force --foreground-scripts appium@next"
            ));
            isFirstInstall = true;
        }

        if (isFirstInstall) {
            ReportManager.logDiscrete("Installing the latest Appium 2.x drivers, and plugins...");

            TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList(
                    "cd " + androidSelfManagedEnvironmentLocation + File.separator + "npm",
                    "appium driver install xcuitest",
                    "appium driver install uiautomator2",
                    "appium plugin install images",
                    "appium plugin install --source=npm appium-dashboard",
                    "appium plugin install --source=npm appium-device-farm"
            ));
        } else {
            ReportManager.logDiscrete("Updating all existing Appium 2.x components (server, drivers, and plugins)...");
            TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList(
                    "cd " + androidSelfManagedEnvironmentLocation + File.separator + "npm",
                    "npm update appium",
                    "appium driver update installed --unsafe",
                    "appium plugin update installed --unsafe"
            ));

            ReportManager.logDiscrete("Checking to see if the required Appium 2.x drivers and plugins are installed...");

            var installedDrivers = TerminalActions.getInstance(false, true).performTerminalCommand("appium driver list --installed");

            //build list of commands
            ArrayList<String> updateCommands = new ArrayList<>();
            if (!installedDrivers.contains("uiautomator2")) {
                updateCommands.add("appium driver install uiautomator2");
            }
            if (!installedDrivers.contains("xcuitest")) {
                updateCommands.add("appium driver install xcuitest");
            }

            var installedPlugins = TerminalActions.getInstance(false, true).performTerminalCommand("appium plugin list --installed");
            if (!installedPlugins.contains("images")) {
                updateCommands.add("appium plugin install images");
            }
            if (!installedPlugins.contains("appium-dashboard")) {
                updateCommands.add("appium plugin install --source=npm appium-dashboard");
            }
            if (!installedPlugins.contains("device-farm")) {
                updateCommands.add("appium plugin install --source=npm appium-device-farm");
            }

            //execute commands
            TerminalActions.getInstance(false, true).performTerminalCommands(updateCommands);
        }
    }

    @Step("Launching Appium server")
    private void launchAppiumServer() {
        //update appium version variable
        ReportHelper.disableLogging();
        var APPIUM_VERSION = TerminalActions.getInstance(false, false).performTerminalCommand("appium --version");
        ReportHelper.enableLogging();

        ReportManager.logDiscrete("Launching Appium server " + APPIUM_VERSION + "...");

        //set browserstack credentials inside user variables
        TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList("[Environment]::SetEnvironmentVariable('BS_USERNAME', '" + Properties.browserStack.username() + "', 'User')",
                "[Environment]::SetEnvironmentVariable('BS_PASSWORD', '" + Properties.browserStack.accessKey() + "', 'User')"));

        //initialize appium server
        TerminalActions.getInstance(true, true)
                .performTerminalCommands(Arrays.asList("cd " + Properties.paths.properties()
                        , "appium --config .appiumrc.json"));

        // TODO: Refactor to run the below four commands where applicable
        //  npm uninstall --global appium --drivers=xcuitest,uiautomator2 --plugins=images,device-farm,appium-dashboard --global
        //  npm install appium@next --drivers=xcuitest,uiautomator2 --plugins=images --force --foreground-scripts=true --global
        //  appium plugin install --source=npm appium-device-farm
        //  appium plugin install --source=npm appium-dashboard

        ReportManager.log("Appium Device Farm is now up and running: http://" + Properties.platform.executionAddress() + "/dashboard/");
    }
}