package com.shaft.capture.collector;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.browsingcontext.BrowsingContextInfo;
import org.openqa.selenium.bidi.module.BrowsingContextInspector;
import org.openqa.selenium.bidi.module.Script;
import org.openqa.selenium.bidi.script.ChannelValue;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * WebDriver BiDi collector using preload scripts for immediate cross-context signals.
 */
public final class BidiBrowserEventCollector implements BrowserEventCollector {
    private static final String CHANNEL = "shaft-capture";

    private final WebDriver driver;
    private final Map<String, String> promptTypes = new ConcurrentHashMap<>();
    private Script script;
    private BrowsingContextInspector contexts;
    private String preloadId;

    /**
     * Creates a BiDi collector.
     *
     * @param driver BiDi-capable driver
     */
    public BidiBrowserEventCollector(WebDriver driver) {
        if (driver == null) {
            throw new IllegalArgumentException("Capture WebDriver is required.");
        }
        this.driver = driver;
    }

    @Override
    public void start(Consumer<BrowserSignal> signalConsumer, Consumer<String> warningConsumer) {
        if (signalConsumer == null || warningConsumer == null) {
            throw new IllegalArgumentException("Capture signal and warning consumers are required.");
        }
        script = new Script(driver);
        script.onMessage(message -> {
            if (!CHANNEL.equals(message.getChannel())) {
                return;
            }
            Object value = message.getData().getValue().orElse("");
            String contextId = message.getSource().getBrowsingContext().orElse("");
            try {
                signalConsumer.accept(BrowserSignal.fromJson(String.valueOf(value), contextId));
            } catch (RuntimeException exception) {
                warningConsumer.accept("A malformed browser interaction signal was ignored.");
            }
        });
        preloadId = script.addPreloadScript(
                BrowserEventScript.preloadFunction(),
                List.of(new ChannelValue(CHANNEL)));

        contexts = new BrowsingContextInspector(driver);
        contexts.onNavigationCommitted(info -> signalConsumer.accept(BrowserSignal.generated(
                "navigation",
                info.getBrowsingContextId(),
                Map.of("url", info.getUrl()),
                Map.of("action", "OPEN"))));
        contexts.onHistoryUpdated(info -> signalConsumer.accept(BrowserSignal.generated(
                "navigation",
                info.getBrowsingContextId(),
                Map.of("url", info.getUrl()),
                Map.of("action", "OPEN"))));
        contexts.onBrowsingContextCreated(info -> {
            if (isTopLevel(info)) {
                signalConsumer.accept(BrowserSignal.generated(
                        "window_open",
                        info.getId(),
                        Map.of("url", info.getUrl()),
                        Map.of()));
            }
        });
        contexts.onBrowsingContextDestroyed(info -> {
            if (isTopLevel(info)) {
                signalConsumer.accept(BrowserSignal.generated(
                        "window_close",
                        info.getId(),
                        Map.of("url", info.getUrl()),
                        Map.of()));
            }
        });
        contexts.onUserPromptOpened(prompt ->
                promptTypes.put(prompt.getBrowsingContextId(), prompt.getType().toString()));
        contexts.onUserPromptClosed(prompt -> {
            String promptType = promptTypes.remove(prompt.getBrowsingContextId());
            signalConsumer.accept(BrowserSignal.generated(
                    "alert",
                    prompt.getBrowsingContextId(),
                    Map.of(),
                    Map.of(
                            "accepted", prompt.getAccepted(),
                            "promptType", promptType == null ? "" : promptType,
                            "text", prompt.getUserText().orElse(""))));
        });
    }

    @Override
    public void close() {
        if (script != null && preloadId != null) {
            try {
                script.removePreloadScript(preloadId);
            } catch (RuntimeException ignored) {
                // The browser may already have crashed or closed.
            }
        }
        if (contexts != null) {
            try {
                contexts.close();
            } catch (RuntimeException ignored) {
                // Best-effort protocol cleanup.
            }
        }
        if (script != null) {
            try {
                script.close();
            } catch (RuntimeException ignored) {
                // Best-effort protocol cleanup.
            }
        }
    }

    private static boolean isTopLevel(BrowsingContextInfo context) {
        return context.getParentBrowsingContext() == null
                || context.getParentBrowsingContext().isBlank();
    }
}
