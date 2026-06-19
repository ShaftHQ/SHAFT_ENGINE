package com.shaft.capture.cli;

import com.shaft.capture.runtime.CaptureStartOptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;

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
                        "--replay"
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
