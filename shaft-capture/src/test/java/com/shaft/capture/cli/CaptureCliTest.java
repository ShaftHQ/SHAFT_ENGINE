package com.shaft.capture.cli;

import com.shaft.capture.runtime.CaptureStartOptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureCliTest {
    @TempDir
    Path temp;

    @Test
    void argumentsParserHandlesValuesFlagsPathsAndValidation() throws Exception {
        Class<?> argumentsClass = Class.forName("com.shaft.capture.cli.CaptureCli$Arguments");
        Object options = invokeStatic(
                argumentsClass,
                "parse",
                new Class<?>[] {String[].class},
                (Object) new String[] {
                        "--runtime-dir", temp.toString(),
                        "--session", "capture.json",
                        "--headless",
                        "--replay",
                        "--enable-fallback-locators",
                        "--control-flow-preview",
                        "--apply-control-flow-preview", "approved-preview.json"
                });

        assertEquals(temp.toString(), invoke(
                options,
                "required",
                new Class<?>[] {String.class},
                "runtime-dir"));
        assertEquals("fallback", invoke(
                options,
                "value",
                new Class<?>[] {String.class, String.class},
                "missing",
                "fallback"));
        assertEquals(temp, invoke(
                options,
                "path",
                new Class<?>[] {String.class, Path.class},
                "runtime-dir",
                Path.of("fallback")));
        assertEquals(Path.of("capture.json"), invoke(
                options,
                "pathRequired",
                new Class<?>[] {String.class},
                "session"));
        assertEquals(true, invoke(
                options,
                "flag",
                new Class<?>[] {String.class},
                "headless"));
        assertEquals(true, invoke(
                options,
                "flag",
                new Class<?>[] {String.class},
                "enable-fallback-locators"));
        assertEquals(true, invoke(
                options,
                "flag",
                new Class<?>[] {String.class},
                "control-flow-preview"));
        assertEquals(Path.of("approved-preview.json"), invoke(
                options,
                "pathRequired",
                new Class<?>[] {String.class},
                "apply-control-flow-preview"));

        InvocationTargetException missingValue = assertThrows(InvocationTargetException.class,
                () -> invokeStatic(
                        argumentsClass,
                        "parse",
                        new Class<?>[] {String[].class},
                        (Object) new String[] {"--runtime-dir"}));
        InvocationTargetException unexpectedArgument = assertThrows(InvocationTargetException.class,
                () -> invokeStatic(
                        argumentsClass,
                        "parse",
                        new Class<?>[] {String[].class},
                        (Object) new String[] {"runtime-dir"}));
        InvocationTargetException missingRequired = assertThrows(InvocationTargetException.class,
                () -> invoke(
                        options,
                        "required",
                        new Class<?>[] {String.class},
                        "url"));

        assertInstanceOf(IllegalArgumentException.class, missingValue.getCause());
        assertInstanceOf(IllegalArgumentException.class, unexpectedArgument.getCause());
        assertInstanceOf(IllegalArgumentException.class, missingRequired.getCause());
    }

    @Test
    void privateHelpersNormalizeClasspathTokensAndRejectInvalidNumbers() throws Exception {
        Constructor<CaptureCli> constructor = CaptureCli.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        InvocationTargetException constructorFailure =
                assertThrows(InvocationTargetException.class, constructor::newInstance);

        String normalized = (String) invokeStatic(
                "absoluteClassPath",
                new Class<?>[] {String.class},
                "target/classes" + File.pathSeparator + ".");
        String token = (String) invokeStatic("token", new Class<?>[] {});
        String usage = (String) invokeStatic("usage", new Class<?>[] {});
        String fallbackMessage = (String) invokeStatic(
                "safeMessage",
                new Class<?>[] {RuntimeException.class},
                new IllegalStateException(""));
        boolean running = (boolean) invokeStatic(
                "isRunning",
                new Class<?>[] {com.shaft.capture.runtime.CaptureStatus.State.class},
                com.shaft.capture.runtime.CaptureStatus.State.ACTIVE);
        long parsed = (long) invokeStatic(
                "parsePositiveLong",
                new Class<?>[] {String.class, String.class},
                "12",
                "timeout");
        InvocationTargetException invalidNumber = assertThrows(InvocationTargetException.class,
                () -> invokeStatic(
                        "parsePositiveLong",
                        new Class<?>[] {String.class, String.class},
                        "-1",
                        "timeout"));

        assertInstanceOf(IllegalStateException.class, constructorFailure.getCause());
        for (String entry : normalized.split(java.util.regex.Pattern.quote(File.pathSeparator))) {
            assertTrue(Path.of(entry).isAbsolute(), entry);
        }
        assertTrue(token.length() >= 32);
        assertFalse(token.contains("="));
        assertTrue(usage.contains("capture start"));
        assertTrue(usage.contains("features"));
        assertTrue(usage.contains("--enable-fallback-locators"));
        assertTrue(usage.contains("--backend webdriver|playwright"));
        assertTrue(usage.contains("--target-source"));
        assertTrue(usage.contains("--insert-after"));
        assertTrue(usage.contains("--control-flow-preview"));
        assertTrue(usage.contains("--apply-control-flow-preview"));
        assertTrue(usage.contains("FLOW_START"));
        assertEquals("operation failed.", fallbackMessage);
        assertTrue(running);
        assertEquals(12L, parsed);
        assertInstanceOf(IllegalArgumentException.class, invalidNumber.getCause());
    }

    @Test
    void startOptionsAcceptPlaywrightCodegenFlags() throws Exception {
        Class<?> argumentsClass = Class.forName("com.shaft.capture.cli.CaptureCli$Arguments");
        Object options = invokeStatic(
                argumentsClass,
                "parse",
                new Class<?>[] {String[].class},
                (Object) new String[] {
                        "--target", "java",
                        "--test-id-attribute", "data-pw",
                        "--viewport-size", "800,600",
                        "--ignore-https-errors",
                        "--user-agent", "agent",
                        "--timeout", "1500",
                        "--user-data-dir", temp.resolve("profile").toString()
                });

        CaptureStartOptions startOptions = (CaptureStartOptions) invokeStatic(
                "startOptions",
                new Class<?>[] {argumentsClass},
                options);

        assertEquals("java", startOptions.targetLanguage());
        assertEquals("data-pw", startOptions.testIdAttributes().getFirst());
        assertEquals(800, startOptions.viewport().width());
        assertEquals(600, startOptions.viewport().height());
        assertEquals(true, startOptions.ignoreHttpsErrors());
        assertEquals("agent", startOptions.userAgent());
        assertEquals(java.time.Duration.ofMillis(1500), startOptions.timeout());
        assertEquals(temp.resolve("profile").toAbsolutePath().normalize(), startOptions.userDataDirectory());
    }

    @Test
    void generationBackendSelectsPlaywrightAndRejectsUnknownValues() throws Exception {
        Class<?> argumentsClass = Class.forName("com.shaft.capture.cli.CaptureCli$Arguments");
        Object defaultOptions = invokeStatic(
                argumentsClass,
                "parse",
                new Class<?>[] {String[].class},
                (Object) new String[] {});
        Object playwrightOptions = invokeStatic(
                argumentsClass,
                "parse",
                new Class<?>[] {String[].class},
                (Object) new String[] {"--backend", "playwright"});
        Object seleniumOptions = invokeStatic(
                argumentsClass,
                "parse",
                new Class<?>[] {String[].class},
                (Object) new String[] {"--backend", "selenium"});
        Object invalidOptions = invokeStatic(
                argumentsClass,
                "parse",
                new Class<?>[] {String[].class},
                (Object) new String[] {"--backend", "puppeteer"});

        assertEquals("WEBDRIVER", ((Enum<?>) invokeStatic(
                "generationBackend",
                new Class<?>[] {argumentsClass},
                defaultOptions)).name());
        assertEquals("PLAYWRIGHT", ((Enum<?>) invokeStatic(
                "generationBackend",
                new Class<?>[] {argumentsClass},
                playwrightOptions)).name());
        assertEquals("WEBDRIVER", ((Enum<?>) invokeStatic(
                "generationBackend",
                new Class<?>[] {argumentsClass},
                seleniumOptions)).name());

        InvocationTargetException invalidBackend = assertThrows(InvocationTargetException.class,
                () -> invokeStatic(
                        "generationBackend",
                        new Class<?>[] {argumentsClass},
                        invalidOptions));
        assertInstanceOf(IllegalArgumentException.class, invalidBackend.getCause());
    }

    @Test
    void defaultLaunchPrefixRelaunchesViaArgFileToAvoidWindowsCommandLineLimit() throws Exception {
        // A test JVM runs from a multi-entry classpath; passed inline via -cp instead of an
        // @argfile, hundreds of runtime jar paths can exceed Windows' ~32K CreateProcess
        // command-line limit ("The filename or extension is too long"), which fails the daemon
        // relaunch before it starts -- gotcha.windows-jvm-launchers-must-pass-large-classpaths-via-a-java-argfile.
        @SuppressWarnings("unchecked")
        List<String> prefix = (List<String>) invokeStatic("defaultLaunchPrefix", new Class<?>[] {});

        assertEquals(2, prefix.size());
        String argsFileArgument = prefix.get(1);
        assertTrue(argsFileArgument.startsWith("@"), "expected an @argfile launch, got: " + prefix);

        String argsFileContent = java.nio.file.Files.readString(Path.of(argsFileArgument.substring(1)));
        assertTrue(argsFileContent.contains("-cp"));
        assertTrue(argsFileContent.contains(CaptureCli.class.getName()));
    }

    /**
     * Issue #4029: the promoted, documented quick-start flow ({@code shaft-cli codegen} /
     * {@code capture generate} with no {@code --replay}) must keep its exit-0 contract for scripts
     * that grep {@code $?} -- but stdout must carry an unmissable line stating the result is
     * UNCONFIRMED, since a skipped replay never proves the recorded scenario actually runs.
     * Runs the CLI as a real child process: {@code CaptureCli}'s {@code OUTPUT}/{@code ERROR}
     * writers bind directly to {@link java.io.FileDescriptor#out}/{@code err}, bypassing
     * {@code System.out} entirely, so only a real process's captured stdout proves what a user
     * actually sees.
     */
    @Test
    void generateCommandExitsZeroButPrintsUnconfirmedWhenReplayIsSkipped() throws Exception {
        Path session = temp.resolve("capture.json");
        Files.copy(repositoryRoot().resolve(
                "shaft-capture/src/test/resources/fixtures/golden-session-1.0.json"), session);
        Path output = temp.resolve("generated");

        List<String> command = new ArrayList<>();
        command.add(ProcessHandle.current().info().command().orElseThrow());
        command.add("-cp");
        command.add(System.getProperty("java.class.path"));
        command.add(CaptureCli.class.getName());
        command.add("generate");
        command.add("--session");
        command.add(session.toString());
        command.add("--output-dir");
        command.add(output.toString());

        Process process = new ProcessBuilder(command)
                .redirectErrorStream(true)
                .start();
        String stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        boolean finished = process.waitFor(60, TimeUnit.SECONDS);

        assertTrue(finished, "generate command did not finish in time: " + stdout);
        assertEquals(0, process.exitValue(), "a skipped replay must keep the exit-0 quick-start contract: " + stdout);
        assertTrue(stdout.contains("UNCONFIRMED"),
                "stdout must loudly state the result is unconfirmed/not replay-verified: " + stdout);
    }

    private static Path repositoryRoot() {
        Path current = Path.of("").toAbsolutePath().normalize();
        while (current != null) {
            if (Files.isRegularFile(current.resolve("pom.xml")) && Files.isDirectory(current.resolve("shaft-capture"))) {
                return current;
            }
            current = current.getParent();
        }
        throw new IllegalStateException("Repository root could not be resolved.");
    }

    private static Object invokeStatic(String methodName, Class<?>[] parameterTypes, Object... arguments)
            throws Exception {
        return invokeStatic(CaptureCli.class, methodName, parameterTypes, arguments);
    }

    private static Object invokeStatic(Class<?> type, String methodName, Class<?>[] parameterTypes, Object... arguments)
            throws Exception {
        Method method = type.getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(null, arguments);
    }

    private static Object invoke(Object target, String methodName, Class<?>[] parameterTypes, Object... arguments)
            throws Exception {
        Method method = target.getClass().getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(target, arguments);
    }
}
