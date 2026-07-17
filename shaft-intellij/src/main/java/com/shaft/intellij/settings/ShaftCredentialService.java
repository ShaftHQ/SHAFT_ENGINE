package com.shaft.intellij.settings;

import com.intellij.credentialStore.CredentialAttributes;
import com.intellij.credentialStore.Credentials;
import com.intellij.ide.passwordSafe.PasswordSafe;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.shaft.intellij.mcp.ShaftPluginExecutor;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.function.BiConsumer;

/**
 * Stores optional provider keys in IntelliJ Password Safe.
 */
public final class ShaftCredentialService {
    private static final String SERVICE_PREFIX = "SHAFT Autobot ";

    /**
     * Every provider id the plugin ever writes to Password Safe, enumerated in one place so a
     * factory reset (see {@code ShaftPluginResetService}) can clear all of them without missing one.
     */
    public static final List<String> KNOWN_PROVIDERS = List.of(
            "OPENAI_API_KEY",
            "ANTHROPIC_API_KEY",
            "GEMINI_API_KEY",
            "GITHUB_TOKEN");

    private final Executor backgroundExecutor;
    private final Executor edtExecutor;

    public ShaftCredentialService() {
        this(ShaftPluginExecutor.getInstance().executor(), edtOrInline());
    }

    /**
     * Test seam: accepts explicit background/EDT executors so unit tests can exercise the
     * threading behavior of the {@code *Async} methods (issue #3623) without a live IntelliJ
     * {@code Application}/{@code PasswordSafe}. Not public API.
     *
     * @param backgroundExecutor executor the blocking Password Safe I/O runs on
     * @param edtExecutor executor the completion hop runs on (the real EDT in production)
     */
    ShaftCredentialService(Executor backgroundExecutor, Executor edtExecutor) {
        this.backgroundExecutor = backgroundExecutor;
        this.edtExecutor = edtExecutor;
    }

    /**
     * Returns the application-level credential service.
     *
     * @return credential service
     */
    public static ShaftCredentialService getInstance() {
        return ApplicationManager.getApplication().getService(ShaftCredentialService.class);
    }

    /**
     * Resolves the EDT dispatch executor for the no-arg constructor: {@code invokeLater} when a
     * live IntelliJ {@code Application} is running, or synchronous inline execution in headless
     * contexts (mirrors the {@code ApplicationManager.getApplication() == null} fallback idiom
     * already used by {@code ShaftPluginExecutor.getInstance()}'s {@code HeadlessHolder} and
     * {@code GuidedWorkflowPanel.resolveSettings()}).
     *
     * @return executor that dispatches to the EDT, or runs inline when headless
     */
    private static Executor edtOrInline() {
        return runnable -> {
            if (ApplicationManager.getApplication() != null) {
                ApplicationManager.getApplication().invokeLater(runnable);
            } else {
                runnable.run();
            }
        };
    }

    /**
     * Runs blocking background work off the EDT, then hops back onto the EDT once it completes.
     * Test seam for #3623: a test can call this directly with a fake "background work" body to
     * assert threading without needing a live {@code PasswordSafe}/{@code Application}, mirroring
     * the {@link #clearAll(BiConsumer)} split's testability motivation.
     *
     * @param backgroundWork work to run on {@link #backgroundExecutor}
     * @return future completing on {@link #edtExecutor} once {@code backgroundWork} finishes
     */
    CompletableFuture<Void> dispatchAsync(Runnable backgroundWork) {
        return CompletableFuture.runAsync(backgroundWork, backgroundExecutor)
                .thenRunAsync(() -> { }, edtExecutor);
    }

    /**
     * Asynchronous, EDT-safe equivalent of {@link #setApiKey(String, char[])} (issue #3623):
     * callers on the EDT must use this instead of the synchronous method.
     *
     * @param provider provider id
     * @param secret API key characters
     * @return future completing on the EDT once the key is stored
     */
    public CompletableFuture<Void> setApiKeyAsync(String provider, char[] secret) {
        return dispatchAsync(() -> setApiKey(provider, secret));
    }

    /**
     * Asynchronous, EDT-safe equivalent of {@link #apiKey(String)} (issue #3623): callers on the
     * EDT must use this instead of the synchronous method.
     *
     * @param provider provider id
     * @return future completing on the EDT with the API key or empty string
     */
    public CompletableFuture<String> apiKeyAsync(String provider) {
        return CompletableFuture.supplyAsync(() -> apiKey(provider), backgroundExecutor)
                .thenApplyAsync(value -> value, edtExecutor);
    }

    /**
     * Asynchronous, EDT-safe equivalent of {@link #hasApiKey(String)} (issue #3623): callers on
     * the EDT must use this instead of the synchronous method.
     *
     * @param provider provider id
     * @return future completing on the EDT with whether a non-blank API key is stored
     */
    public CompletableFuture<Boolean> hasApiKeyAsync(String provider) {
        return CompletableFuture.supplyAsync(() -> hasApiKey(provider), backgroundExecutor)
                .thenApplyAsync(value -> value, edtExecutor);
    }

    /**
     * Asynchronous, EDT-safe equivalent of {@link #clearAll()} (issue #3623): callers on the EDT
     * must use this instead of the synchronous method.
     *
     * @return future completing on the EDT once every known provider key is cleared
     */
    public CompletableFuture<Void> clearAllAsync() {
        return dispatchAsync(this::clearAll);
    }

    /**
     * Stores or clears a provider API key.
     *
     * <p>Blocking Password Safe I/O (issue #3623): callers on the EDT must use
     * {@link #setApiKeyAsync(String, char[])} instead.
     *
     * @param provider provider id
     * @param secret API key characters
     */
    @RequiresBackgroundThread
    public void setApiKey(String provider, char[] secret) {
        try {
            String value = secret == null ? "" : new String(secret);
            PasswordSafe.getInstance().set(attributes(provider),
                    value.isBlank() ? null : new Credentials(provider, value));
        } finally {
            if (secret != null) {
                Arrays.fill(secret, '\0');
            }
        }
    }

    /**
     * Reads a provider API key.
     *
     * <p>Blocking Password Safe I/O (issue #3623): callers on the EDT must use
     * {@link #apiKeyAsync(String)} instead.
     *
     * @param provider provider id
     * @return API key or empty string
     */
    @RequiresBackgroundThread
    public String apiKey(String provider) {
        Credentials credentials = PasswordSafe.getInstance().get(attributes(provider));
        return credentials == null ? "" : credentials.getPasswordAsString();
    }

    /**
     * Indicates whether a provider API key is currently stored.
     *
     * <p>Blocking Password Safe I/O (issue #3623): callers on the EDT must use
     * {@link #hasApiKeyAsync(String)} instead.
     *
     * @param provider provider id
     * @return true if a non-blank API key is stored for the provider
     */
    @RequiresBackgroundThread
    public boolean hasApiKey(String provider) {
        Credentials credentials = PasswordSafe.getInstance().get(attributes(provider));
        if (credentials == null) {
            return false;
        }
        String password = credentials.getPasswordAsString();
        return password != null && !password.isBlank();
    }

    /**
     * Clears every provider key the plugin ever writes (see {@link #KNOWN_PROVIDERS}).
     *
     * <p>Blocking Password Safe I/O (issue #3623): callers on the EDT must use
     * {@link #clearAllAsync()} instead.
     */
    @RequiresBackgroundThread
    public void clearAll() {
        clearAll(this::setApiKey);
    }

    /**
     * Runs the clear-all algorithm against the given setter, decoupled from {@link #setApiKey} and
     * Password Safe so the loop over {@link #KNOWN_PROVIDERS} can be executed and verified by a
     * plain unit test (Password Safe requires a running IntelliJ Application).
     *
     * @param setter invoked with each known provider id and a {@code null} secret to clear it
     */
    static void clearAll(BiConsumer<String, char[]> setter) {
        for (String provider : KNOWN_PROVIDERS) {
            setter.accept(provider, null);
        }
    }

    private static CredentialAttributes attributes(String provider) {
        return new CredentialAttributes(SERVICE_PREFIX + provider);
    }
}
