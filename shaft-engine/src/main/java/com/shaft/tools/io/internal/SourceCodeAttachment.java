package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import io.qameta.allure.AllureExternalKey;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.AttachmentOptions;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StepResult;

import java.io.ByteArrayInputStream;
import java.lang.StackWalker.StackFrame;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.IdentityHashMap;
import java.util.ArrayDeque;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

/** Collects bounded source-call evidence across Allure lifecycle callbacks without creating report steps. */
public final class SourceCodeAttachment {
    private static final int MAX_ACTIVE_STEPS = 1024;
    private static final StackWalker WALKER = StackWalker.getInstance(StackWalker.Option.RETAIN_CLASS_REFERENCE);
    // Allure loads separate listener instances for each SPI. Share pending captures by model identity so
    // test/fixture callbacks can release the step listener's state, without mixing equal nested steps.
    // StepResult equality/hashCode change with name/status, so ordinary hash keys are unsafe here.
    private static final Map<StepResult, Capture> active = new IdentityHashMap<>();

    /** Creates a collector for one lifecycle listener's callbacks. */
    public SourceCodeAttachment() { }

    /**
     * Captures the user invocation and explicit step identity, without reading source files.
     *
     * @param step the newly started step
     * @param lifecycleSupplier the listener's owning lifecycle, resolved only when enabled
     */
    public void started(StepResult step, Supplier<AllureLifecycle> lifecycleSupplier) {
        try {
            Mode mode = mode();
            if (mode == Mode.NEVER) {
                return;
            }
            Caller caller = WALKER.walk(frames -> caller(frames.limit(96).toList()));
            if (caller == null) {
                return;
            }
            AllureLifecycle lifecycle = lifecycleSupplier.get();
            var key = lifecycle.getCurrentExecutableKey();
            var root = lifecycle.getCurrentRootKey();
            if (key.isEmpty() || root.isEmpty()) {
                return;
            }
            AtomicBoolean sameStep = new AtomicBoolean();
            lifecycle.updateStep(key.get(), current -> sameStep.set(current == step));
            if (sameStep.get()) {
                synchronized (active) {
                    if (active.size() < MAX_ACTIVE_STEPS) {
                        active.put(step, new Capture(caller, mode, lifecycle, key.get()));
                    }
                }
            }
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete(exception);
        }
    }

    /**
     * Adds selected source evidence directly to the captured step, even when another step is current.
     *
     * @param step the step whose final status is known
     */
    public void stopping(StepResult step) {
        Capture capture;
        synchronized (active) {
            capture = active.remove(step);
        }
        if (capture == null) {
            return;
        }
        try {
            if (!capture.mode().includes(step.getStatus(), capture.caller().validation()) || mode() == Mode.NEVER) {
                return;
            }
            Caller caller = capture.caller();
            Path source = SourceCodeContext.findSourceFile(caller.frame(), caller.type());
            SourceCodeContext.Window window = SourceCodeContext.read(source, caller.frame().getLineNumber());
            String location = source == null ? fileName(caller.frame()) : SourceCodeContext.relative(source, caller.type());
            String html = render(caller.frame(), location, window);
            String name = "Source code — " + fileName(caller.frame())
                    + (caller.frame().getLineNumber() > 0 ? ":" + caller.frame().getLineNumber() : "");
            capture.lifecycle().addAttachment(capture.stepKey(), ReportSecrets.redact(name), "text/html",
                    new ByteArrayInputStream(html.getBytes(StandardCharsets.UTF_8)),
                    AttachmentOptions.withFileExtension(".html"));
        } catch (RuntimeException exception) {
            // Evidence must never change the action outcome. Log discretely to avoid reporting recursion.
            ReportManagerHelper.logDiscrete(exception);
        }
    }

    /**
     * Releases any abandoned captures when their owning test or fixture ends.
     *
     * @param steps the stopping test or fixture's own steps
     */
    public void clear(List<StepResult> steps) {
        synchronized (active) {
            if (active.isEmpty()) {
                return;
            }
            ArrayDeque<StepResult> pending = new ArrayDeque<>(steps);
            while (!pending.isEmpty() && !active.isEmpty()) {
                StepResult step = pending.removeFirst();
                active.remove(step);
                pending.addAll(step.getSteps());
            }
        }
    }

    private static Mode mode() {
        String value = SHAFT.Properties.reporting.whenToAttachSourceCode();
        if (value == null) {
            return Mode.NEVER;
        }
        return switch (value.trim().toLowerCase(Locale.ROOT)) {
            case "always" -> Mode.ALWAYS;
            case "validationpointsonly" -> Mode.VALIDATIONS;
            case "failuresonly" -> Mode.FAILURES;
            default -> Mode.NEVER;
        };
    }

    private static Caller caller(List<StackFrame> frames) {
        boolean action = false;
        boolean validation = false;
        for (StackFrame frame : frames) {
            String name = frame.getClassName();
            if (engineFrame(frame)) {
                if ((name.equals(ReportManagerHelper.class.getName()) && frame.getMethodName().equals("attachAsStep"))
                        || (name.equals(ReportManager.class.getName()) && frame.getMethodName().equals("log"))
                        || name.equals(AttachmentReporter.class.getName()) || name.equals(AllureAttachments.class.getName())) {
                    return null;
                }
                validation |= name.startsWith("com.shaft.validation.")
                        || name.startsWith("com.shaft.gui.playwright.validation.")
                        || (name.equals(ReportManagerHelper.class.getName())
                        && frame.getMethodName().equals("logNestedSteps") && frame.getDescriptor().contains("CheckpointType"));
                action |= (name.startsWith("com.shaft.gui.") || name.startsWith("com.shaft.api.")
                        || name.startsWith("com.shaft.cli.") || name.startsWith("com.shaft.db."))
                        && (name.contains("Actions") || name.contains("BrowserDownload"));
            } else if (!infrastructure(name)) {
                return action || validation ? new Caller(frame.toStackTraceElement(), frame.getDeclaringClass(), validation) : null;
            }
        }
        return null;
    }

    private static boolean engineFrame(StackFrame frame) {
        return frame.getClassName().startsWith("com.shaft.")
                && Objects.equals(frame.getDeclaringClass().getProtectionDomain().getCodeSource(),
                SourceCodeAttachment.class.getProtectionDomain().getCodeSource());
    }

    private static boolean infrastructure(String name) {
        return name.startsWith("java.") || name.startsWith("javax.") || name.startsWith("jdk.")
                || name.startsWith("sun.") || name.startsWith("io.qameta.") || name.startsWith("org.aspectj.")
                || name.startsWith("org.testng.") || name.startsWith("org.junit.")
                || name.startsWith("org.apache.maven.") || name.startsWith("org.gradle.")
                || name.startsWith("org.mockito.") || name.startsWith("net.bytebuddy.");
    }

    private static String fileName(StackTraceElement frame) {
        String name = frame.getFileName();
        return name == null || name.contains("/") || name.contains("\\") ? "Source unavailable" : name;
    }

    static String render(StackTraceElement frame, String location, SourceCodeContext.Window window) {
        StringBuilder html = new StringBuilder("""
                <!doctype html><html lang="en"><head><meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <title>Source code</title><style>
                :root{color-scheme:light dark}body{margin:0;padding:16px;font:14px system-ui,sans-serif}
                h1{font-size:16px;margin:0 0 4px}p{margin:4px 0 12px;overflow-wrap:anywhere}
                pre{padding:12px 0;border:1px solid #888;border-radius:6px;overflow-x:auto;tab-size:4}
                code{font:13px/1.7 ui-monospace,Consolas,monospace}
                .source-line{display:block;padding:0 12px;white-space:pre-wrap;overflow-wrap:anywhere}
                .active{background:#fff2b2;color:#171717;border-left:3px solid #8a6300;padding-left:9px}
                .number{display:inline-block;min-width:5ch;text-align:right;margin-right:2ch;user-select:none}
                .arrow{display:inline-block;width:2ch}.note{font-size:12px}
                </style></head><body><h1>Source code</h1><p>
                """);
        html.append(escape(redact(location))).append("</p><p>")
                .append(escape(redact(frame.getClassName() + "." + frame.getMethodName())))
                .append("</p>");
        if (window.lines().isEmpty()) {
            html.append("<p>").append(escape(window.unavailableReason())).append("</p>");
            if (frame.getLineNumber() > 0) {
                html.append("<p>Recorded line: ").append(frame.getLineNumber()).append("</p>");
            }
        } else {
            String original = String.join("\n", window.lines().stream().map(SourceCodeContext.Line::text).toList());
            String[] redacted = redact(original).split("\n", -1);
            if (redacted.length != window.lines().size()) {
                html.append("<p>Source context omitted because it contains sensitive content.</p>");
            } else {
                html.append("<pre><code>");
                for (int i = 0; i < redacted.length; i++) {
                    int number = window.lines().get(i).number();
                    boolean current = number == frame.getLineNumber();
                    html.append("<span class=\"source-line").append(current ? " active" : "").append("\">")
                            .append("<span class=\"arrow\">").append(current ? "&#8594;" : " ")
                            .append("</span><span class=\"number\">").append(number).append("</span>")
                            .append(escape(redacted[i])).append("</span>");
                }
                html.append("</code></pre>");
            }
        }
        return html.append("<p class=\"note\">The arrow marks the recorded call-site line. Surrounding lines are context, not proof of execution.</p></body></html>")
                .toString();
    }

    private static String redact(String text) {
        return ReportSecrets.redact(FailureTraceReporter.redactSourceText(text));
    }

    private static String escape(String text) {
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                .replace("\"", "&quot;").replace("'", "&#39;");
    }

    private enum Mode {
        NEVER, FAILURES, VALIDATIONS, ALWAYS;

        boolean includes(Status status, boolean validation) {
            boolean failure = status == Status.FAILED || status == Status.BROKEN;
            return status != null && status != Status.SKIPPED
                    && (this == ALWAYS || (this == FAILURES && failure) || (this == VALIDATIONS && (validation || failure)));
        }
    }

    private record Caller(StackTraceElement frame, Class<?> type, boolean validation) { }
    private record Capture(Caller caller, Mode mode, AllureLifecycle lifecycle, AllureExternalKey stepKey) { }
}
