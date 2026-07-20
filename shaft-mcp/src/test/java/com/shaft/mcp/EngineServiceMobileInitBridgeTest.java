package com.shaft.mcp;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * {@code driver_initialize} absorbs {@code mobile_initialize_native}/
 * {@code mobile_initialize_web_emulation} via an optional {@code mobileOptions} nested object
 * (design doc amendment A9). Mirrors {@link EngineServiceCaptureBridgeTest}'s static-bridge pattern
 * so {@link EngineService} never needs a direct {@link MobileService} dependency (which would form a
 * constructor-injection cycle, since {@link MobileService} already depends on {@link EngineService}).
 */
class EngineServiceMobileInitBridgeTest {
    @AfterEach
    void unregisterBridge() {
        EngineService.registerMobileInitBridge(null);
    }

    @Test
    void initializeDriverDispatchesMobileNativeThroughTheBridge() {
        java.util.concurrent.atomic.AtomicReference<ActiveEngine> seenEngine = new java.util.concurrent.atomic.AtomicReference<>();
        java.util.concurrent.atomic.AtomicReference<McpMobileInitOptions> seenOptions = new java.util.concurrent.atomic.AtomicReference<>();
        EngineService.registerMobileInitBridge((engine, options) -> {
            seenEngine.set(engine);
            seenOptions.set(options);
        });
        McpMobileInitOptions options = new McpMobileInitOptions(
                null, null, "Pixel 6", null, null, null, null, null,
                "Android", null, null, null, null, null, null, null, null);

        new EngineService().initializeDriver(null, ActiveEngine.MOBILE_NATIVE, options);

        assertEquals(ActiveEngine.MOBILE_NATIVE, seenEngine.get());
        assertEquals("Android", seenOptions.get().platformName());
    }

    @Test
    void initializeDriverDispatchesMobileWebThroughTheBridgeAndDefaultsOptionsWhenOmitted() {
        java.util.concurrent.atomic.AtomicReference<ActiveEngine> seenEngine = new java.util.concurrent.atomic.AtomicReference<>();
        java.util.concurrent.atomic.AtomicReference<McpMobileInitOptions> seenOptions = new java.util.concurrent.atomic.AtomicReference<>();
        EngineService.registerMobileInitBridge((engine, options) -> {
            seenEngine.set(engine);
            seenOptions.set(options);
        });

        new EngineService().initializeDriver(null, ActiveEngine.MOBILE_WEB, null);

        assertEquals(ActiveEngine.MOBILE_WEB, seenEngine.get());
        assertEquals(McpMobileInitOptions.EMPTY, seenOptions.get());
    }

    @Test
    void initializeDriverExplainsMissingBridgeInsteadOfThrowingNpe() {
        IllegalStateException failure = assertThrows(IllegalStateException.class,
                () -> new EngineService().initializeDriver(null, ActiveEngine.MOBILE_NATIVE, null));

        assertEquals(true, failure.getMessage().contains("mobile"));
    }
}
