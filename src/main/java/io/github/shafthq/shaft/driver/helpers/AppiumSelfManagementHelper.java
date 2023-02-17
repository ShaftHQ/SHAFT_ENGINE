package io.github.shafthq.shaft.driver.helpers;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.properties.Properties;
import io.github.shafthq.shaft.tools.io.helpers.ReportHelper;
import io.qameta.allure.Step;
import lombok.Getter;
import lombok.SneakyThrows;
import org.apache.commons.lang3.SystemUtils;
import org.testng.Assert;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static io.github.shafthq.shaft.driver.helpers.DriverFactoryHelper.showProgressBar;

public class AppiumSelfManagementHelper {
    // TODO: implement new environment variables for dockerized instance (or remove the code)
    @Getter
    private static final boolean appiumDockerizedExecution = false;
    private static AppiumSelfManagementHelper singleInstance = null;
    private final String cliToolsVersion = "9477386_latest";
    private final String nodeJsVersion = "v18.14.0";
    @Getter
    private static final boolean terminateAppiumContainersAfterExecution = false;

    private static final long longDownloadTimeout = TimeUnit.MINUTES.toSeconds(45); // seconds
    private final long downloadTimeout = TimeUnit.MINUTES.toSeconds(10); // seconds
    private static final String androidEmulatorLocation = "src/main/resources/docker-compose/android-emulator/";
    private final String androidSelfManagedEnvironmentLocation = System.getProperty("user.home") + File.separator + ".shaft" + File.separator + "android" + File.separator;
    private final String subpathToBin = "cmdline-tools" + File.separator + "latest" + File.separator + "bin";
    private final String subpathToPlatform = "platform-tools";

    @SneakyThrows(InterruptedException.class)
    private AppiumSelfManagementHelper() {
        System.setProperty("videoParams_recordVideo", "true");
        // TODO: check if user has admin access
        // TODO: create a list to manage URLs for other OSs

        //check that they are installed and can be executed
        ReportManager.logDiscrete("Verifying Appium self-managed execution environment...");

        // FORK to SETUP ANDROID SDK CLI AND SETUP APPIUM SERVER IN PARALLEL
//        ScheduledExecutorService prepareAppiumSelfManagedEnvironment = Executors.newScheduledThreadPool(2);
//        if (Properties.platform.targetOperatingSystem().equals(OperatingSystems.ANDROID)) {
//            prepareAppiumSelfManagedEnvironment.execute(() -> {
//                try {
        setupAndroidSDKCLITools();
//                } catch (InterruptedException e) {
//                    throw new RuntimeException(e);
//                }
        setupAndroidSDKPlatformTools();
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

//        if (!prepareAppiumSelfManagedEnvironment.awaitTermination(longDownloadTimeout, TimeUnit.SECONDS)) {
//            prepareAppiumSelfManagedEnvironment.shutdownNow();
//            Assert.fail("Appium self-managed execution environment was still not ready after " + TimeUnit.SECONDS.toMinutes(longDownloadTimeout) + " minutes.");
//        }
    }

    @Step("Setting up Appium self-managed execution environment")
    public static AppiumSelfManagementHelper setupAppiumSelfManagedExecutionPrerequisites() {
        if (singleInstance == null) {
            singleInstance = new AppiumSelfManagementHelper();
        }
        return singleInstance;
    }

    @SneakyThrows(java.lang.InterruptedException.class)
    @Step("Downloading Android emulator files")
    public static void downloadAndroidEmulatorFiles() {
        ReportManager.logDiscrete("Downloading https://github.com/amrsa1/Android-Emulator container files...");
        ReportHelper.disableLogging();
        // https://github.com/amrsa1/Android-Emulator
        var downloadableFileURLs = Arrays.asList(
                "https://raw.githubusercontent.com/amrsa1/Android-Emulator/main/Dockerfile",
                "https://raw.githubusercontent.com/amrsa1/Android-Emulator/main/docker-compose.yml",
                "https://raw.githubusercontent.com/amrsa1/Android-Emulator/main/start_appium.sh",
                "https://raw.githubusercontent.com/amrsa1/Android-Emulator/main/start_emu_headless.sh",
                "https://raw.githubusercontent.com/amrsa1/Android-Emulator/main/start_vnc.sh",
                "https://raw.githubusercontent.com/amrsa1/Android-Emulator/main/start_emu.sh");
        downloadableFileURLs.forEach(url -> FileActions.getInstance().downloadFile(url, androidEmulatorLocation + url.substring(url.lastIndexOf("/") + 1)));
        ReportHelper.enableLogging();

        ReportManager.logDiscrete("Customizing container configuration...");
        ReportHelper.disableLogging();

        // Edit file to fix System UI isn't responding
        // https://github.com/actions/runner-images/issues/2741
        // https://github.com/amrsa1/Android-Emulator/issues/2
//        https://github.com/akv-platform/espressodemo/blob/main/.github/workflows/blank.yml
//        https://github.com/actions/runner-images/issues/3719
        FileActions.getInstance().writeToFile(androidEmulatorLocation, "start_emu_headless.sh",
                FileActions.getInstance().readFile("src/main/resources/docker-compose/", "start_emu_headless"));

        String appWithPath = SHAFT.Properties.mobile.app();
        if (!"".equals(appWithPath)) {
            SHAFT.Properties.mobile.set().app(appWithPath.substring(appWithPath.lastIndexOf("/")));
            SHAFT.Properties.platform.set().executionAddress("localhost:4725");
        }
        // https://github.com/appium/appium/issues/12287
        System.setProperty("mobile_uiautomator2ServerInstallTimeout", "1200000");
        System.setProperty("mobile_uiautomator2ServerLaunchTimeout", "1200000");
        System.setProperty("mobile_adbExecTimeout", "1200000");
        ReportHelper.enableLogging();
        //TODO: execute command to ensure that docker desktop/platform is installed and running
        // else fail fast
        ReportManager.logDiscrete("Launching Android-Emulator and Appium 2 containers. If the containers aren't on your machine they may take some time to download (5.57 GB) depending on your internet connection...");
        ReportHelper.disableLogging();
        var logMessage = "with container id: ";

        ScheduledExecutorService stage1Executor = Executors.newScheduledThreadPool(2);
        stage1Executor.execute(() -> showProgressBar("Fetching containers", longDownloadTimeout));
        stage1Executor.schedule(() -> {
            ReportHelper.disableLogging();
            try {
                executeCommand("docker compose up --scale android-service=1 --detach --wait --no-recreate --remove-orphans");
                ReportHelper.enableLogging();
                ReportManager.logDiscrete("Successfully prepared docker image.");
                stage1Executor.shutdownNow();
            } catch (Throwable throwable) {
                ReportHelper.enableLogging();
                stage1Executor.shutdownNow();
                Assert.fail("Failed to prepare docker image.", throwable);
            }
        }, 0, TimeUnit.SECONDS);

        if (!stage1Executor.awaitTermination(longDownloadTimeout, TimeUnit.SECONDS)) {
            ReportHelper.enableLogging();
            Assert.fail("Docker image was still not ready after " + TimeUnit.SECONDS.toMinutes(longDownloadTimeout) + " minutes.");
        }

        ReportHelper.disableLogging();
        logMessage = "Successfully initialized Android-Emulator and Appium 2 containerized instance " + logMessage;
        var commandLog = executeCommand("docker ps -q");
        var runningContainerID = commandLog.substring(commandLog.lastIndexOf("\n")).trim();

        ReportHelper.enableLogging();
        ReportManager.logDiscrete(logMessage + runningContainerID);

        if (!"".equals(appWithPath)) {
            ReportManager.logDiscrete("Transferring " + SHAFT.Properties.mobile.app().replace("/", "") + " to target container...");
            ReportHelper.disableLogging();
            // copy .apk to container root
            executeCommand("docker cp " + FileActions.getInstance().getAbsolutePath(appWithPath) + " " + runningContainerID + ":/");
            // make .apk editable
            executeCommand("docker exec -d android-emulator chmod u+x " + SHAFT.Properties.mobile.app());
            ReportHelper.enableLogging();
        }
    }

    @Step("Setting up Android SDK CLI tools")
    private void setupAndroidSDKCLITools() throws InterruptedException {
        ReportManager.logDiscrete("Verifying Android SDK CLI tools...");
        // using an already existing android_home will cause issues upgrading due to lack of admin access.
//        String ANDROID_HOME = System.getenv("ANDROID_HOME");
//        System.setProperty("ANDROID_HOME", ANDROID_HOME);

        // setting a static and self-managed android home directory is the better approach to fully control all downloaded binaries
        System.setProperty("ANDROID_HOME", androidSelfManagedEnvironmentLocation);

        if (!FileActions.getInstance().doesFileExist(System.getProperty("ANDROID_HOME") + subpathToBin)) {
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

            ScheduledExecutorService sdkCmdToolsDownloader = Executors.newScheduledThreadPool(2);
            sdkCmdToolsDownloader.execute(() -> showProgressBar("Preparing Android SDK CLI Tools", downloadTimeout));
            String finalUrl = url;
            sdkCmdToolsDownloader.schedule(() -> {
                ReportHelper.disableLogging();
                try {
                    FileActions.getInstance().unpackArchive(new URL(finalUrl), androidSelfManagedEnvironmentLocation);
                    FileActions.getInstance().copyFolder(androidSelfManagedEnvironmentLocation + "cmdline-tools" + File.separator + "bin", androidSelfManagedEnvironmentLocation + subpathToBin);
                    ReportManager.logDiscrete("Successfully prepared Android SDK CLI tools.");
                    sdkCmdToolsDownloader.shutdownNow();
                } catch (Throwable throwable) {
                    ReportHelper.enableLogging();
                    sdkCmdToolsDownloader.shutdownNow();
                    Assert.fail("Failed to prepare Android SDK CLI tools.", throwable);
                }
            }, 0, TimeUnit.SECONDS);

            if (!sdkCmdToolsDownloader.awaitTermination(downloadTimeout, TimeUnit.SECONDS)) {
                ReportHelper.enableLogging();
                sdkCmdToolsDownloader.shutdownNow();
                Assert.fail("Android SDK CLI tools were still not ready after " + TimeUnit.SECONDS.toMinutes(downloadTimeout) + " minutes.");
            }
//            // this only runs if the thread succeeds
//            System.setProperty("ANDROID_HOME", androidSelfManagedEnvironmentLocation);
        }

        ReportManager.logDiscrete("Found Android SDK CLI Tools at " + System.getProperty("ANDROID_HOME") + subpathToBin);
    }

    @Step("Setting up Android SDK Platform tools")
    private void setupAndroidSDKPlatformTools() throws InterruptedException {
        ReportManager.logDiscrete("Verifying Android SDK Platform tools...");

        if (!FileActions.getInstance().doesFileExist(System.getProperty("ANDROID_HOME") + subpathToPlatform)) {
            String url = "https://dl.google.com/android/repository/platform-tools-latest-${OS}.zip";
            if (SystemUtils.IS_OS_MAC) {
                url = url.replace("${OS}", "darwin");
            } else if (SystemUtils.IS_OS_WINDOWS) {
                url = url.replace("${OS}", "windows");
            } else {
                //default is linux
                url = url.replace("${OS}", "linux");
            }
            ReportManager.logDiscrete("Downloading and installing Android SDK Platform Tools from " + url);

            ScheduledExecutorService sdkCmdToolsDownloader = Executors.newScheduledThreadPool(2);
            sdkCmdToolsDownloader.execute(() -> showProgressBar("Preparing Android SDK Platform Tools", downloadTimeout));
            String finalUrl = url;
            sdkCmdToolsDownloader.schedule(() -> {
                ReportHelper.disableLogging();
                try {
                    FileActions.getInstance().unpackArchive(new URL(finalUrl), androidSelfManagedEnvironmentLocation);
//                    FileActions.getInstance().copyFolder(androidSelfManagedEnvironmentLocation+"cmdline-tools"+File.separator+"bin", androidSelfManagedEnvironmentLocation+subpathToBin);
                    ReportManager.logDiscrete("Successfully prepared Android SDK Platform tools.");
                    sdkCmdToolsDownloader.shutdownNow();
                } catch (Throwable throwable) {
                    ReportHelper.enableLogging();
                    sdkCmdToolsDownloader.shutdownNow();
                    Assert.fail("Failed to prepare Android SDK Platform tools.", throwable);
                }
            }, 0, TimeUnit.SECONDS);

            if (!sdkCmdToolsDownloader.awaitTermination(downloadTimeout, TimeUnit.SECONDS)) {
                ReportHelper.enableLogging();
                sdkCmdToolsDownloader.shutdownNow();
                Assert.fail("Android SDK Platform tools were still not ready after " + TimeUnit.SECONDS.toMinutes(downloadTimeout) + " minutes.");
            }
//            // this only runs if the thread succeeds
//            System.setProperty("ANDROID_HOME", androidSelfManagedEnvironmentLocation);
        }

        ReportManager.logDiscrete("Found Android SDK Platform Tools at " + System.getProperty("ANDROID_HOME") + subpathToPlatform);
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
        if (!path.contains(ANDROID_HOME + subpathToBin + ";")) path = ANDROID_HOME + subpathToBin + ";" + path;
        if (!path.contains(ANDROID_HOME + subpathToPlatform + ";"))
            path = ANDROID_HOME + subpathToPlatform + ";" + path;
        if (!path.contains(ANDROID_HOME + "bin;")) path = ANDROID_HOME + "bin;" + path;
        if (!path.contains(ANDROID_HOME + "bin;")) path = ANDROID_HOME + "lib;" + path;

        // if nothing is augmented, then this would only reset the path variable without messing it up
        LinkedList<String> setEnvironmentVariables = new LinkedList<>();
        setEnvironmentVariables.addAll(Arrays.asList(
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
                LinkedList<String> finalCommands = commands;
                packages.forEach(packageName -> {
                    finalCommands.add("echo \"y\" | sdkmanager --install " + packageName);
                });
                TerminalActions.getInstance(false, true).performTerminalCommands(finalCommands);
                // accepting licenses
                TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')",
                        "echo \"y\" | sdkmanager --licenses"));
                // creating avd
                TerminalActions.getInstance(false, true).performTerminalCommands(Arrays.asList("$Env:Path = [System.Environment]::GetEnvironmentVariable('Path','User')",
                        "avdmanager  create avd -n " + getAvdName() + " -d pixel --package " + packages.get(packages.size() - 1)));
                ReportManager.logDiscrete("Successfully prepared android packages.");
            } catch (Throwable throwable) {
                Assert.fail("Failed to prepare android packages.", throwable);
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
            Assert.fail("Failed to launch emulator.", throwable);
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
                FileActions.getInstance().unpackArchive(new URL(url), androidSelfManagedEnvironmentLocation + "npm" + File.separator);
                ReportManager.logDiscrete("Successfully prepared Node.js.");
            } catch (Throwable throwable) {
                Assert.fail("Failed to prepare Node.js.", throwable);
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
        if ("".equals(APPIUM_VERSION) || APPIUM_VERSION.contains("The term 'appium' is not recognized") || !APPIUM_VERSION.startsWith("2.")) {
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

        ReportManager.log("Appium Device Farm is now up and running: http://" + Properties.platform.executionAddress() + "/dashboard/");
    }

    public static void terminateAppiumContainers() {
        ReportManager.logDiscrete("Terminating Appium containers...");
        ReportHelper.disableLogging();
        executeCommand("docker rm -f android-service", true);
        ReportHelper.enableLogging();
    }

    private static String executeCommand(String command) {
        return executeCommand(command, false);
    }

    private static String executeCommand(String command, boolean asynchronous) {
        String consoleOutput;
        String fileName = command.substring(0, command.indexOf(" ", 7)).replaceAll(" ", "_");
        var setExecutionLocationCommand = "cd '" + androidEmulatorLocation + "'\n";
        if (SystemUtils.IS_OS_WINDOWS) {
            FileActions.getInstance().writeToFile(AppiumSelfManagementHelper.androidEmulatorLocation, fileName + ".bat", setExecutionLocationCommand + command);
            consoleOutput = new TerminalActions(asynchronous).performTerminalCommand(AppiumSelfManagementHelper.androidEmulatorLocation + fileName + ".bat");
            FileActions.getInstance().deleteFile(AppiumSelfManagementHelper.androidEmulatorLocation + fileName + ".bat");
        } else {
            FileActions.getInstance().writeToFile(AppiumSelfManagementHelper.androidEmulatorLocation, fileName + ".sh", setExecutionLocationCommand + command);
            new TerminalActions(asynchronous).performTerminalCommand("chmod u+x " + AppiumSelfManagementHelper.androidEmulatorLocation + fileName + ".sh");
            consoleOutput = new TerminalActions(asynchronous).performTerminalCommand("sh " + AppiumSelfManagementHelper.androidEmulatorLocation + fileName + ".sh");
            FileActions.getInstance().deleteFile(AppiumSelfManagementHelper.androidEmulatorLocation + fileName + ".sh");
        }
        return consoleOutput;
    }

}
