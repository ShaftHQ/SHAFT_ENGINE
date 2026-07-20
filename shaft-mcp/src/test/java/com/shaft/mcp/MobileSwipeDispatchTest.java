package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * The unified {@code mobile_swipe} tool (design doc Decision 2) absorbs
 * {@code mobile_swipe_by_offset}/{@code mobile_swipe_coordinates}/{@code mobile_swipe_element_into_view}/
 * {@code mobile_swipe_text_into_view}: which underlying gesture runs is selected by which optional
 * params are supplied, checked most-specific-first (text, then locator+offset, then locator+direction,
 * then raw coordinates).
 */
class MobileSwipeDispatchTest {
    @TempDir
    Path temp;

    private MobileService service() {
        return spy(new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp)));
    }

    @Test
    void swipeDispatchesToTextIntoViewWhenTextIsSupplied() {
        MobileService service = service();
        org.mockito.Mockito.doReturn(null).when(service).swipeTextIntoView("Find me", "vertical");

        service.swipe(null, null, null, "Find me", "vertical", null, null, null, null, null, null);

        verify(service).swipeTextIntoView("Find me", "vertical");
    }

    @Test
    void swipeDispatchesToByOffsetWhenLocatorAndOffsetsAreSupplied() {
        MobileService service = service();
        org.mockito.Mockito.doReturn(null).when(service)
                .swipeByOffset(locatorStrategy.ID, "list", 10, 20);

        service.swipe(locatorStrategy.ID, "list", null, null, null, 10, 20, null, null, null, null);

        verify(service).swipeByOffset(locatorStrategy.ID, "list", 10, 20);
    }

    @Test
    void swipeDispatchesToElementIntoViewWhenOnlyLocatorIsSupplied() {
        MobileService service = service();
        org.mockito.Mockito.doReturn(null).when(service)
                .swipeElementIntoView(locatorStrategy.ID, "target", "down");

        service.swipe(locatorStrategy.ID, "target", "down", null, null, null, null, null, null, null, null);

        verify(service).swipeElementIntoView(locatorStrategy.ID, "target", "down");
    }

    @Test
    void swipeDispatchesToCoordinatesWhenNoLocatorOrTextIsSupplied() {
        MobileService service = service();
        org.mockito.Mockito.doReturn(null).when(service).swipeCoordinates(1, 2, 3, 4, 100);

        service.swipe(null, null, null, null, null, null, null, 1, 2, 3, 4);

        verify(service).swipeCoordinates(1, 2, 3, 4, 100);
    }

    @Test
    void swipeRejectsAmbiguousCallsWithNeitherLocatorTextNorCoordinates() {
        MobileService service = service();

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.swipe(null, null, null, null, null, null, null, null, null, null, null));

        assertEquals(true, failure.getMessage().contains("mobile_swipe"));
    }
}
