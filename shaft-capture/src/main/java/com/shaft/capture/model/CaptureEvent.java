package com.shaft.capture.model;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;

import java.time.Duration;
import java.util.List;

/**
 * Versioned browser action or explicit verification captured for replay.
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes({
        @JsonSubTypes.Type(value = CaptureEvent.NavigationEvent.class, name = "navigation"),
        @JsonSubTypes.Type(value = CaptureEvent.ClickEvent.class, name = "click"),
        @JsonSubTypes.Type(value = CaptureEvent.TypeEvent.class, name = "type"),
        @JsonSubTypes.Type(value = CaptureEvent.ClearEvent.class, name = "clear"),
        @JsonSubTypes.Type(value = CaptureEvent.SelectEvent.class, name = "select"),
        @JsonSubTypes.Type(value = CaptureEvent.ToggleEvent.class, name = "toggle"),
        @JsonSubTypes.Type(value = CaptureEvent.UploadEvent.class, name = "upload"),
        @JsonSubTypes.Type(value = CaptureEvent.KeyboardEvent.class, name = "keyboard"),
        @JsonSubTypes.Type(value = CaptureEvent.WindowEvent.class, name = "window"),
        @JsonSubTypes.Type(value = CaptureEvent.FrameEvent.class, name = "frame"),
        @JsonSubTypes.Type(value = CaptureEvent.AlertEvent.class, name = "alert"),
        @JsonSubTypes.Type(value = CaptureEvent.WaitEvent.class, name = "wait"),
        @JsonSubTypes.Type(value = CaptureEvent.VerificationEvent.class, name = "verification")
})
public sealed interface CaptureEvent permits CaptureEvent.NavigationEvent, CaptureEvent.ClickEvent,
        CaptureEvent.TypeEvent, CaptureEvent.ClearEvent, CaptureEvent.SelectEvent, CaptureEvent.ToggleEvent,
        CaptureEvent.UploadEvent, CaptureEvent.KeyboardEvent, CaptureEvent.WindowEvent, CaptureEvent.FrameEvent,
        CaptureEvent.AlertEvent, CaptureEvent.WaitEvent, CaptureEvent.VerificationEvent {
    /**
     * Returns common event context.
     *
     * @return event context
     */
    EventContext context();

    /**
     * Navigation actions.
     */
    enum NavigationAction {
        OPEN,
        BACK,
        FORWARD,
        REFRESH
    }

    /**
     * Mouse buttons.
     */
    enum MouseButton {
        PRIMARY,
        MIDDLE,
        SECONDARY
    }

    /**
     * Select-list operations.
     */
    enum SelectMode {
        VALUE,
        VISIBLE_TEXT,
        INDEX
    }

    /**
     * Window and tab operations.
     */
    enum WindowAction {
        OPEN_TAB,
        OPEN_WINDOW,
        SWITCH,
        CLOSE
    }

    /**
     * Frame operations.
     */
    enum FrameAction {
        ENTER,
        EXIT,
        TOP
    }

    /**
     * Alert operations.
     */
    enum AlertAction {
        ACCEPT,
        DISMISS,
        TYPE,
        VERIFY_TEXT
    }

    /**
     * Wait conditions.
     */
    enum WaitCondition {
        ELEMENT_PRESENT,
        ELEMENT_VISIBLE,
        ELEMENT_CLICKABLE,
        ELEMENT_ABSENT,
        URL_CONTAINS,
        TITLE_CONTAINS,
        DOCUMENT_READY,
        FIXED_DURATION
    }

    /**
     * Explicit verification kinds.
     */
    enum VerificationKind {
        ELEMENT_PRESENT,
        ELEMENT_VISIBLE,
        ELEMENT_ENABLED,
        ELEMENT_SELECTED,
        TEXT_EQUALS,
        TEXT_CONTAINS,
        ATTRIBUTE_EQUALS,
        URL_EQUALS,
        URL_CONTAINS,
        TITLE_EQUALS
    }

    /**
     * Navigation event.
     *
     * @param context common event context
     * @param action navigation operation
     * @param targetUrl sanitized target URL when applicable
     */
    @JsonTypeName("navigation")
    record NavigationEvent(EventContext context, NavigationAction action, String targetUrl) implements CaptureEvent {
        /**
         * Creates a navigation event.
         */
        public NavigationEvent {
            requireContext(context);
            action = action == null ? NavigationAction.OPEN : action;
            targetUrl = ModelSupport.text(targetUrl);
            if (action == NavigationAction.OPEN && targetUrl.isBlank()) {
                throw new IllegalArgumentException("Open navigation requires a target URL.");
            }
        }
    }

    /**
     * Click event.
     *
     * @param context common event context
     * @param target action target
     * @param button mouse button
     * @param clickCount click count
     */
    @JsonTypeName("click")
    record ClickEvent(
            EventContext context,
            ElementSnapshot target,
            MouseButton button,
            int clickCount) implements CaptureEvent {
        /**
         * Creates a click event.
         */
        public ClickEvent {
            requireContext(context);
            requireTarget(target);
            button = button == null ? MouseButton.PRIMARY : button;
            if (clickCount < 1) {
                throw new IllegalArgumentException("Click count must be positive.");
            }
        }
    }

    /**
     * Typing event. The original value is never stored in this model.
     *
     * @param context common event context
     * @param target action target
     * @param value external data reference
     */
    @JsonTypeName("type")
    record TypeEvent(
            EventContext context,
            ElementSnapshot target,
            ExternalTestDataReference value) implements CaptureEvent {
        /**
         * Creates a typing event.
         */
        public TypeEvent {
            requireContext(context);
            requireTarget(target);
            requireValue(value);
        }
    }

    /**
     * Clear event.
     *
     * @param context common event context
     * @param target action target
     */
    @JsonTypeName("clear")
    record ClearEvent(EventContext context, ElementSnapshot target) implements CaptureEvent {
        /**
         * Creates a clear event.
         */
        public ClearEvent {
            requireContext(context);
            requireTarget(target);
        }
    }

    /**
     * Select-list event.
     *
     * @param context common event context
     * @param target action target
     * @param mode selection mode
     * @param value external data reference
     */
    @JsonTypeName("select")
    record SelectEvent(
            EventContext context,
            ElementSnapshot target,
            SelectMode mode,
            ExternalTestDataReference value) implements CaptureEvent {
        /**
         * Creates a select event.
         */
        public SelectEvent {
            requireContext(context);
            requireTarget(target);
            mode = mode == null ? SelectMode.VALUE : mode;
            requireValue(value);
        }
    }

    /**
     * Check or uncheck event.
     *
     * @param context common event context
     * @param target action target
     * @param checked desired checked state
     */
    @JsonTypeName("toggle")
    record ToggleEvent(EventContext context, ElementSnapshot target, boolean checked) implements CaptureEvent {
        /**
         * Creates a toggle event.
         */
        public ToggleEvent {
            requireContext(context);
            requireTarget(target);
        }
    }

    /**
     * Upload event containing only a logical file reference and safe metadata.
     *
     * @param context common event context
     * @param target action target
     * @param file logical file reference
     * @param safeFileName sanitized filename
     * @param mediaType media type
     * @param sizeBytes optional file size
     */
    @JsonTypeName("upload")
    record UploadEvent(
            EventContext context,
            ElementSnapshot target,
            ExternalTestDataReference file,
            String safeFileName,
            String mediaType,
            long sizeBytes) implements CaptureEvent {
        /**
         * Creates an upload event.
         */
        public UploadEvent {
            requireContext(context);
            requireTarget(target);
            requireValue(file);
            safeFileName = ModelSupport.requireText(safeFileName, "Safe upload filename");
            if (safeFileName.contains("/") || safeFileName.contains("\\")) {
                throw new IllegalArgumentException("Upload filename cannot contain a path.");
            }
            mediaType = ModelSupport.text(mediaType);
            if (sizeBytes < 0) {
                throw new IllegalArgumentException("Upload size cannot be negative.");
            }
        }
    }

    /**
     * Keyboard event.
     *
     * @param context common event context
     * @param target optional focused target
     * @param keys normalized key names
     */
    @JsonTypeName("keyboard")
    record KeyboardEvent(
            EventContext context,
            ElementSnapshot target,
            List<String> keys) implements CaptureEvent {
        /**
         * Creates a keyboard event.
         */
        public KeyboardEvent {
            requireContext(context);
            keys = ModelSupport.list(keys);
            if (keys.isEmpty()) {
                throw new IllegalArgumentException("Keyboard event requires at least one key.");
            }
        }
    }

    /**
     * Window or tab event.
     *
     * @param context common event context
     * @param action window operation
     * @param logicalWindowId target logical identifier
     */
    @JsonTypeName("window")
    record WindowEvent(
            EventContext context,
            WindowAction action,
            String logicalWindowId) implements CaptureEvent {
        /**
         * Creates a window event.
         */
        public WindowEvent {
            requireContext(context);
            action = action == null ? WindowAction.SWITCH : action;
            logicalWindowId = ModelSupport.requireText(logicalWindowId, "Logical window ID");
        }
    }

    /**
     * Frame event.
     *
     * @param context common event context
     * @param action frame operation
     * @param logicalFrameId logical frame identifier
     * @param target optional frame element
     */
    @JsonTypeName("frame")
    record FrameEvent(
            EventContext context,
            FrameAction action,
            String logicalFrameId,
            ElementSnapshot target) implements CaptureEvent {
        /**
         * Creates a frame event.
         */
        public FrameEvent {
            requireContext(context);
            action = action == null ? FrameAction.ENTER : action;
            logicalFrameId = ModelSupport.text(logicalFrameId);
            if (action == FrameAction.ENTER && logicalFrameId.isBlank() && target == null) {
                throw new IllegalArgumentException("Entering a frame requires a logical ID or target.");
            }
        }
    }

    /**
     * Browser alert event.
     *
     * @param context common event context
     * @param action alert operation
     * @param text optional external text reference
     */
    @JsonTypeName("alert")
    record AlertEvent(
            EventContext context,
            AlertAction action,
            ExternalTestDataReference text) implements CaptureEvent {
        /**
         * Creates an alert event.
         */
        public AlertEvent {
            requireContext(context);
            action = action == null ? AlertAction.ACCEPT : action;
            if ((action == AlertAction.TYPE || action == AlertAction.VERIFY_TEXT) && text == null) {
                throw new IllegalArgumentException("Alert text operation requires an external data reference.");
            }
        }
    }

    /**
     * Explicit wait event.
     *
     * @param context common event context
     * @param condition wait condition
     * @param timeout maximum wait duration
     * @param target optional target
     * @param expected optional external expected value
     */
    @JsonTypeName("wait")
    record WaitEvent(
            EventContext context,
            WaitCondition condition,
            Duration timeout,
            ElementSnapshot target,
            ExternalTestDataReference expected) implements CaptureEvent {
        /**
         * Creates a wait event.
         */
        public WaitEvent {
            requireContext(context);
            condition = condition == null ? WaitCondition.DOCUMENT_READY : condition;
            timeout = timeout == null ? Duration.ofSeconds(30) : timeout;
            if (timeout.isNegative() || timeout.isZero()) {
                throw new IllegalArgumentException("Wait timeout must be positive.");
            }
        }
    }

    /**
     * Explicit verification event.
     *
     * @param context common event context
     * @param verification verification kind
     * @param target optional target
     * @param expected optional external expected value
     * @param negated whether the verification is negated
     */
    @JsonTypeName("verification")
    record VerificationEvent(
            EventContext context,
            VerificationKind verification,
            ElementSnapshot target,
            ExternalTestDataReference expected,
            boolean negated) implements CaptureEvent {
        /**
         * Creates a verification event.
         */
        public VerificationEvent {
            requireContext(context);
            verification = verification == null ? VerificationKind.ELEMENT_PRESENT : verification;
        }
    }

    private static void requireContext(EventContext context) {
        if (context == null) {
            throw new IllegalArgumentException("Event context is required.");
        }
    }

    private static void requireTarget(ElementSnapshot target) {
        if (target == null) {
            throw new IllegalArgumentException("Event target is required.");
        }
    }

    private static void requireValue(ExternalTestDataReference value) {
        if (value == null) {
            throw new IllegalArgumentException("External test-data reference is required.");
        }
    }
}
