package com.shaft.gui.mobile;

import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileContextActionsContract;
import com.shaft.tools.io.internal.MobileTraceMetadata;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.remote.SupportsContextSwitching;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Native and hybrid Appium context actions. */
final class ContextActions implements MobileContextActionsContract {
    private static final String NATIVE_APP = "NATIVE_APP";
    private final MobileActions mobile;

    ContextActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public String current() {
        return query("current", "", () -> contexts().getContext());
    }

    @Override
    public List<String> handles() {
        return query("handles", "", () -> List.copyOf(new ArrayList<>(contexts().getContextHandles())));
    }

    @Override
    public MobileContextActionsContract switchTo(String context) {
        String requested = context == null || context.isBlank() ? "<context>" : context;
        query("switch-to", requested, () -> {
            String validated = requireContext(context);
            contexts().context(validated);
            return validated;
        });
        return this;
    }

    @Override
    public MobileContextActionsContract nativeApp() {
        return switchTo(NATIVE_APP);
    }

    @Override
    public MobileContextActionsContract webView() {
        query("switch-to-webview", "<first-webview>", () -> {
            SupportsContextSwitching provider = contexts();
            String webView = provider.getContextHandles().stream()
                    .filter(Objects::nonNull)
                    .filter(handle -> handle.toUpperCase(Locale.ROOT).startsWith("WEBVIEW"))
                    .findFirst()
                    .orElseThrow(() -> new UnsupportedOperationException(
                            "The live Appium session has no available web-view context."));
            provider.context(webView);
            return webView;
        });
        return this;
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private SupportsContextSwitching contexts() {
        if (mobile.driver() instanceof SupportsContextSwitching provider) {
            return provider;
        }
        throw new UnsupportedOperationException(
                "The live Appium session does not support native or web context switching.");
    }

    private <T> T query(String operation, String requestedContext, Supplier<T> action) {
        var event = TraceEventRecorder.start("mobile/context", operation, requestedContext, mobile.traceDriver());
        String before = safeCurrent();
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile context action completed.", null,
                    metadata(requestedContext, before, safeCurrent(), result, false), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "Mobile context action failed.", exception,
                    metadata(requestedContext, before, safeCurrent(), null, true), List.of());
            throw exception;
        }
    }

    private Map<String, String> metadata(String requestedContext, String before, String after,
                                         Object result, boolean includeNativeSource) {
        Map<String, String> metadata = new LinkedHashMap<>(
                MobileTraceMetadata.mobileMetadata(mobile.traceDriver(), includeNativeSource));
        metadata.put("requestedContext", requestedContext);
        metadata.put("contextBefore", before);
        metadata.put("contextAfter", after);
        if (result != null) {
            metadata.put("result", String.valueOf(result));
        }
        return metadata;
    }

    private String safeCurrent() {
        try {
            if (mobile.traceDriver().getSessionId() == null
                    || !(mobile.traceDriver() instanceof SupportsContextSwitching provider)) {
                return "unavailable";
            }
            String current = provider.getContext();
            return current == null ? "unavailable" : current;
        } catch (RuntimeException ignored) {
            return "unavailable";
        }
    }

    private static String requireContext(String context) {
        if (context == null || context.isBlank()) {
            throw new IllegalArgumentException("The mobile context must not be blank.");
        }
        return context;
    }
}
