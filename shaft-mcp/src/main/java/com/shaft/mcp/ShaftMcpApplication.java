package com.shaft.mcp;

import com.shaft.capture.cli.CaptureCli;
import com.shaft.doctor.cli.DoctorCli;
import com.shaft.mcp.install.ShaftMcpInstaller;
import org.springframework.ai.support.ToolCallbacks;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
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
        if (args.length > 0 && "install".equalsIgnoreCase(args[0])) {
            int exitCode = ShaftMcpInstaller.run(Arrays.copyOfRange(args, 1, args.length));
            if (exitCode != 0) {
                System.exit(exitCode);
            }
            return;
        }
        new SpringApplication(ShaftMcpApplication.class).run(args);
	}

    /**
     * Registers the ShaftService tool callbacks.
     * @param engineService the ShaftService instance
     * @return a list of ToolCallback instances
     */
	@Bean
	public List<ToolCallback> shaftTools(
            EngineService engineService,
            BrowserService browserService,
            ElementService elementService,
            NaturalActionService naturalActionService,
            MobileService mobileService,
            CaptureService captureService,
            DoctorService doctorService) {
        var engineServiceList = List.of(ToolCallbacks.from(engineService));
        var browserServiceList = List.of(ToolCallbacks.from(browserService));
        var elementServiceList = List.of(ToolCallbacks.from(elementService));
        var naturalActionServiceList = List.of(ToolCallbacks.from(naturalActionService));
        var mobileServiceList = List.of(ToolCallbacks.from(mobileService));
        var captureServiceList = List.of(ToolCallbacks.from(captureService));
        var doctorServiceList = List.of(ToolCallbacks.from(doctorService));

        var serviceList = new java.util.ArrayList<ToolCallback>();
        serviceList.addAll(engineServiceList);
        serviceList.addAll(browserServiceList);
        serviceList.addAll(elementServiceList);
        serviceList.addAll(naturalActionServiceList);
        serviceList.addAll(mobileServiceList);
        serviceList.addAll(captureServiceList);
        serviceList.addAll(doctorServiceList);
        return serviceList;
	}

    private static List<String> captureLaunchPrefix() {
        String javaCommand = ProcessHandle.current().info().command().orElse("java");
        String[] processArguments = ProcessHandle.current().info().arguments().orElse(new String[0]);
        for (int index = 0; index + 1 < processArguments.length; index++) {
            if ("-jar".equals(processArguments[index])) {
                return List.of(javaCommand, "-jar", processArguments[index + 1], "capture");
            }
        }
        String classPath = ManagementFactory.getRuntimeMXBean().getClassPath();
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
        return List.of(
                javaCommand,
                "-cp",
                absoluteClassPath(classPath),
                CaptureCli.class.getName());
    }

    private static String absoluteClassPath(String classPath) {
        return Arrays.stream(classPath.split(Pattern.quote(File.pathSeparator)))
                .map(entry -> Path.of(entry).toAbsolutePath().normalize().toString())
                .collect(Collectors.joining(File.pathSeparator));
    }
}
