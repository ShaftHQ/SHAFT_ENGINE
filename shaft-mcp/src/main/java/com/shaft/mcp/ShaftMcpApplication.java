package com.shaft.mcp;

import com.shaft.capture.cli.CaptureCli;
import com.shaft.doctor.cli.DoctorCli;
import org.springframework.ai.support.ToolCallbacks;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@SpringBootApplication
public class ShaftMcpApplication {

    /**
     * The main entry point for the ShaftMcpApplication.
     * @param args command-line arguments
     */
	public static void main(String[] args) throws IOException {
        if (args.length > 0 && "doctor".equalsIgnoreCase(args[0])) {
            int exitCode = DoctorCli.run(Arrays.copyOfRange(args, 1, args.length));
            if (exitCode != 0) {
                System.exit(exitCode);
            }
            return;
        }
        if (args.length > 0 && "capture".equalsIgnoreCase(args[0])) {
            int exitCode = CaptureCli.run(
                    Arrays.copyOfRange(args, 1, args.length),
                    captureLaunchPrefix());
            if (exitCode != 0) {
                System.exit(exitCode);
            }
            return;
        }
        new SpringApplication(ShaftMcpApplication.class).run(args);
	}

    /**
     * Registers the ShaftService tool callbacks. {@link PlaywrightService} and
     * {@link NaturalActionService}'s former {@code natural_act}/{@code mobile_natural_act} tools are
     * deliberately absent: since commit 4 of the tool architecture sweep (design doc Decision 2),
     * every {@code playwright_*} tool is absorbed into the unified {@code element_*}/{@code browser_*}/
     * {@code capture_*}/{@code doctor_*}/{@code healer_*} tools (PlaywrightService is still a Spring
     * bean, injected directly by those services as a dispatch target -- it registers zero
     * {@code @Tool} methods of its own), and the two intent-based "smart act" tools were deleted
     * outright per an explicit owner mandate (no deprecation shim, no re-export).
     * @param engineService the ShaftService instance
     * @return a list of ToolCallback instances
     */
	@Bean
	public List<ToolCallback> shaftTools(
            EngineService engineService,
            BrowserService browserService,
            ElementService elementService,
            MobileService mobileService,
            CaptureService captureService,
            DoctorService doctorService,
            TraceService traceService,
            HealerService healerService,
            GuideService guideService,
            ShaftProjectService shaftProjectService,
            TestAutomationService testAutomationService,
            CodingPartnerService codingPartnerService,
            AutobotService autobotService,
            PlannerService plannerService) {
        var engineServiceList = List.of(ToolCallbacks.from(engineService));
        var browserServiceList = List.of(ToolCallbacks.from(browserService));
        var elementServiceList = List.of(ToolCallbacks.from(elementService));
        var mobileServiceList = List.of(ToolCallbacks.from(mobileService));
        var captureServiceList = List.of(ToolCallbacks.from(captureService));
        var doctorServiceList = List.of(ToolCallbacks.from(doctorService));
        var traceServiceList = List.of(ToolCallbacks.from(traceService));
        var healerServiceList = List.of(ToolCallbacks.from(healerService));
        var guideServiceList = List.of(ToolCallbacks.from(guideService));
        var shaftProjectServiceList = List.of(ToolCallbacks.from(shaftProjectService));
        var testAutomationServiceList = List.of(ToolCallbacks.from(testAutomationService));
        var codingPartnerServiceList = List.of(ToolCallbacks.from(codingPartnerService));
        var autobotServiceList = List.of(ToolCallbacks.from(autobotService));
        var plannerServiceList = List.of(ToolCallbacks.from(plannerService));

        var serviceList = new java.util.ArrayList<ToolCallback>();
        serviceList.addAll(engineServiceList);
        serviceList.addAll(browserServiceList);
        serviceList.addAll(elementServiceList);
        serviceList.addAll(mobileServiceList);
        serviceList.addAll(captureServiceList);
        serviceList.addAll(doctorServiceList);
        serviceList.addAll(traceServiceList);
        serviceList.addAll(healerServiceList);
        serviceList.addAll(guideServiceList);
        serviceList.addAll(shaftProjectServiceList);
        serviceList.addAll(testAutomationServiceList);
        serviceList.addAll(codingPartnerServiceList);
        serviceList.addAll(autobotServiceList);
        serviceList.addAll(plannerServiceList);
        return serviceList;
	}

    private static List<String> captureLaunchPrefix() throws IOException {
        return captureLaunchPrefix(
                ProcessHandle.current().info().arguments().orElse(new String[0]),
                ManagementFactory.getRuntimeMXBean().getClassPath());
    }

    /**
     * Builds the relaunch command from explicit launch inputs: the current JVM's launch shape is
     * not a reliable signal inside tests, where Surefire forks through a manifest-only booter jar
     * ({@code -jar surefirebooter*.jar}) instead of the multi-entry classpath dev/IDE runs use.
     */
    private static List<String> captureLaunchPrefix(String[] processArguments, String classPath) throws IOException {
        String javaCommand = ProcessHandle.current().info().command().orElse("java");
        for (int index = 0; index + 1 < processArguments.length; index++) {
            if ("-jar".equals(processArguments[index])) {
                return List.of(javaCommand, "-jar", processArguments[index + 1], "capture");
            }
        }
        String[] classPathEntries = classPath.split(Pattern.quote(File.pathSeparator));
        if (classPathEntries.length == 1) {
            Path executable = Path.of(classPathEntries[0]).toAbsolutePath().normalize();
            if (Files.isRegularFile(executable) && executable.getFileName().toString().endsWith(".jar")) {
                return List.of(javaCommand, "-jar", executable.toString(), "capture");
            }
        }
        try {
            Path application = Path.of(ShaftMcpApplication.class.getProtectionDomain()
                    .getCodeSource().getLocation().toURI());
            if (Files.isRegularFile(application) && application.getFileName().toString().endsWith(".jar")) {
                return List.of(javaCommand, "-jar", application.toString(), "capture");
            }
        } catch (Exception ignored) {
            // Development and test runs use the current classpath below.
        }
        return List.of(javaCommand,
                "@" + writeLaunchArgsFile(absoluteClassPath(classPath), CaptureCli.class.getName()));
    }

    private static String absoluteClassPath(String classPath) {
        return Arrays.stream(classPath.split(Pattern.quote(File.pathSeparator)))
                .map(entry -> Path.of(entry).toAbsolutePath().normalize().toString())
                .collect(Collectors.joining(File.pathSeparator));
    }

    /**
     * Writes a Java {@code @argfile} for relaunching with the given classpath: passed inline on
     * Windows, hundreds of runtime jar paths can exceed the ~32K CreateProcess command-line limit
     * ("The filename or extension is too long"), which fails the capture daemon relaunch before it
     * starts. Only reached when this JVM itself wasn't started with {@code -jar} (dev/IDE runs).
     */
    private static Path writeLaunchArgsFile(String classpath, String mainClass) throws IOException {
        Path argsFile = Files.createTempFile("shaft-mcp-capture-launch-", ".args");
        argsFile.toFile().deleteOnExit();
        Files.writeString(argsFile,
                "-cp" + System.lineSeparator()
                        + argFileQuote(classpath) + System.lineSeparator()
                        + mainClass + System.lineSeparator(),
                StandardCharsets.UTF_8);
        return argsFile.toAbsolutePath().normalize();
    }

    /**
     * Quotes one Java @argfile line: backslashes become forward slashes (the argfile parser
     * treats backslashes as escapes) and embedded quotes are escaped.
     */
    private static String argFileQuote(String value) {
        return '"' + value.replace("\\", "/").replace("\"", "\\\"") + '"';
    }
}
