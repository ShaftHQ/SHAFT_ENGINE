package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class McpServiceHelperTest {
    @TempDir
    Path temp;

    @AfterEach
    void cleanup() {
        SHAFT.Properties.clearForCurrentThread();
        new EngineService().quitDriver();
    }

    @Test
    void mobileActionResultHelperRecordsSnippetWhenRecorderIsActive() throws Exception {
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        service.recordStart(temp.resolve("mobile-actions.json").toString(), "native", true);

        McpMobileActionResult result = (McpMobileActionResult) invoke(
                service,
                "actionResult",
                new Class<?>[] {
                        String.class,
                        locatorStrategy.class,
                        String.class,
                        Map.class,
                        String.class,
                        String.class,
                        boolean.class
                },
                "custom",
                locatorStrategy.ID,
                "submit",
                Map.of("key", "value"),
                "driver.touch().tap(By.id(\"submit\"));",
                "driver.touch().tap(By.id(\"submit\"));",
                false);

        assertEquals("custom", result.action());
        assertTrue(result.recorded());
        assertTrue(result.codeBlock().code().contains("driver.touch().tap"));
        assertTrue(service.recordStop(false).actionCount() > 0);
    }

    @Test
    void mobileHelperWritesScreenshotsAndBuildsCoordinateSnippets() throws Exception {
        MobileService service = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        byte[] png = {1, 2, 3};

        Path written = (Path) invoke(
                service,
                "writeScreenshot",
                new Class<?>[] {String.class, byte[].class},
                "screens/mobile.png",
                png);
        String tapCode = (String) invokeStatic(
                MobileService.class,
                "tapCoordinatesCode",
                new Class<?>[] {int.class, int.class},
                10,
                20);
        String swipeCode = (String) invokeStatic(
                MobileService.class,
                "swipeCoordinatesCode",
                new Class<?>[] {int.class, int.class, int.class, int.class, int.class},
                1,
                2,
                3,
                4,
                1);
        int parsed = (int) invokeStatic(
                MobileService.class,
                "integer",
                new Class<?>[] {Map.class, String.class},
                Map.of("durationMillis", "125"),
                "durationMillis");

        assertArrayEquals(png, Files.readAllBytes(written));
        assertTrue(tapCode.contains("10, 20"));
        assertTrue(swipeCode.contains("Duration.ofMillis(100)"));
        assertEquals(125, parsed);
    }

    @Test
    void mobileHelperRejectsReplayWhenSensitiveValueWasRedacted() {
        McpMobileRecordedAction action = new McpMobileRecordedAction(
                1,
                "",
                "type",
                "ID",
                "username",
                Map.of("value", "<redacted>"),
                "driver.element().type(By.id(\"username\"), \"<redacted>\");",
                false,
                null);

        InvocationTargetException failure = assertThrows(InvocationTargetException.class,
                () -> invokeStatic(
                        MobileService.class,
                        "requireSensitive",
                        new Class<?>[] {McpMobileRecordedAction.class},
                        action));

        assertInstanceOf(IllegalArgumentException.class, failure.getCause());
        assertTrue(failure.getCause().getMessage().contains("omitted sensitive values"));
    }

    @Test
    void browserScreenshotHelperWritesInsideWorkspaceAndSkipsBlankPath() throws Exception {
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp));
        byte[] png = {9, 8, 7};

        Path written = (Path) invoke(
                service,
                "writeScreenshot",
                new Class<?>[] {String.class, byte[].class},
                "screens/browser.png",
                png);
        Object blank = invoke(
                service,
                "writeScreenshot",
                new Class<?>[] {String.class, byte[].class},
                "",
                png);

        assertArrayEquals(png, Files.readAllBytes(written));
        assertNull(blank);
    }

    @Test
    void elementHelperMethodsHandleNullAndMixedTextValues() throws Exception {
        assertEquals(0, invokeStatic(
                ElementService.class,
                "safeLength",
                new Class<?>[] {String.class},
                new Object[] {null}));
        assertEquals(5, invokeStatic(
                ElementService.class,
                "safeLength",
                new Class<?>[] {String.class},
                "value"));
        assertEquals(6, invokeStatic(
                ElementService.class,
                "totalLength",
                new Class<?>[] {CharSequence[].class},
                (Object) new CharSequence[] {"one", null, "two"}));
    }

    @Test
    void naturalActionSettingsApplyBoundedOverridesAndRestorePreviousValues() throws Exception {
        SHAFT.Properties.naturalActions.set()
                .enabled(false)
                .minimumTrustPercentage(25)
                .planner("deterministic")
                .aiFallbackEnabled(false)
                .allowedActions("browser");
        Class<?> settings = Class.forName("com.shaft.mcp.NaturalActionService$NaturalActionSettings");
        Object previous = invokeStatic(settings, "current", new Class<?>[] {});
        Object overridden = invoke(
                previous,
                "withOverrides",
                new Class<?>[] {Integer.class, String.class, Boolean.class, String.class},
                150,
                " auto ",
                true,
                " element ");

        invoke(overridden, "applyEnabled", new Class<?>[] {});
        assertTrue(SHAFT.Properties.naturalActions.enabled());
        assertEquals(100, SHAFT.Properties.naturalActions.minimumTrustPercentage());
        assertEquals("auto", SHAFT.Properties.naturalActions.planner());
        assertTrue(SHAFT.Properties.naturalActions.aiFallbackEnabled());
        assertEquals("element", SHAFT.Properties.naturalActions.allowedActions());

        invoke(previous, "apply", new Class<?>[] {});
        assertFalse(SHAFT.Properties.naturalActions.enabled());
        assertEquals(25, SHAFT.Properties.naturalActions.minimumTrustPercentage());
        assertEquals("deterministic", SHAFT.Properties.naturalActions.planner());
        assertEquals("fallback", invokeStatic(
                settings,
                "textOrDefault",
                new Class<?>[] {String.class, String.class},
                " ",
                "fallback"));
        assertEquals(0, invokeStatic(
                NaturalActionService.class,
                "safeLength",
                new Class<?>[] {String.class},
                new Object[] {null}));
    }

    @Test
    void runtimePathsConstructorKeepsUtilityClassClosed() throws Exception {
        Constructor<McpRuntimePaths> constructor = McpRuntimePaths.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException failure = assertThrows(InvocationTargetException.class, constructor::newInstance);

        assertInstanceOf(IllegalStateException.class, failure.getCause());
        assertTrue(failure.getCause().getMessage().contains("Utility class"));
    }

    @Test
    void doctorHelpersResolvePathListsAndRejectMalformedDiagnosis() throws Exception {
        @SuppressWarnings("unchecked")
        List<Path> paths = (List<Path>) invokeStatic(
                DoctorService.class,
                "paths",
                new Class<?>[] {List.class},
                List.of("target/report.json", "allure-results"));
        Object configuration = invokeStatic(DoctorService.class, "currentConfiguration", new Class<?>[] {});
        Path malformed = temp.resolve("diagnosis.json");
        Files.writeString(malformed, "not-json");

        InvocationTargetException failure = assertThrows(InvocationTargetException.class,
                () -> invokeStatic(
                        DoctorService.class,
                        "readDiagnosis",
                        new Class<?>[] {Path.class},
                        malformed));

        assertEquals(List.of(Path.of("target/report.json"), Path.of("allure-results")), paths);
        assertTrue(configuration == null || configuration instanceof com.shaft.pilot.config.PilotConfiguration);
        assertInstanceOf(IllegalArgumentException.class, failure.getCause());
        assertTrue(failure.getCause().getMessage().contains("Doctor diagnosis could not be read"));
    }

    private static Object invoke(Object target, String methodName, Class<?>[] parameterTypes, Object... arguments)
            throws Exception {
        Method method = target.getClass().getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(target, arguments);
    }

    private static Object invokeStatic(Class<?> type, String methodName, Class<?>[] parameterTypes, Object... arguments)
            throws Exception {
        Method method = type.getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(null, arguments);
    }
}
