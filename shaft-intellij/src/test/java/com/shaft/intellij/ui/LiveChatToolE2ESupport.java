package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Caret;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.EditorDataProvider;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorComposite;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorNavigatable;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.shaft.intellij.approval.ToolApprovalDecision;
import com.shaft.intellij.approval.ToolApprovalService;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftPluginExecutor;
import com.shaft.intellij.settings.ShaftSettingsState;

import javax.accessibility.AccessibleContext;
import javax.swing.JButton;
import javax.swing.JComponent;
import java.awt.Component;
import java.awt.Container;
import java.lang.reflect.Field;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Test-only bridge that drives {@link ShaftAssistantPanel#send} exactly the way the real IDE does
 * -- typing a prompt into the composer, then clicking the real "Send" button -- against a REAL
 * spawned SHAFT MCP server process (issue #3872/#3866 T6).
 *
 * <p>Closing the gap this fixes: {@code ShaftMcpInvocationService.startTool}'s 3-arg overload
 * (the one {@code ShaftAssistantPanel#dispatchApprovedTool} calls) unconditionally reads {@code
 * ShaftSettingsState.getInstance()}, which unconditionally reads {@code
 * ApplicationManager.getApplication()} with no null guard ({@code ShaftSettingsState.java:23}).
 * shaft-intellij's Gradle test JVM has no {@code com.intellij.testFramework} on its classpath (no
 * {@code testFramework(...)} declaration in {@code build.gradle.kts}), so every existing test
 * either dodges {@code ShaftSettingsState.getInstance()} via a package-private
 * explicit-{@code Settings} overload, or traps at the {@code Project.getService(...)} boundary so
 * the real service (and the {@code ApplicationManager} calls inside it) is never reached -- see
 * {@code ShaftAssistantPanelToolsCacheWarmupTest} and {@code ShaftAssistantPanelToolCardTest}'s
 * class docs, which say so explicitly. Nothing in shaft-intellij faked {@code Application} itself
 * before this.</p>
 *
 * <p>{@code com.intellij.openapi.application.Application} is a plain interface (extends {@code
 * ComponentManager}), so a {@link Proxy} answers it the same way every other shaft-intellij test
 * already fakes {@code Project} (see {@code ShaftAssistantPromptRoutingTest#fakeProject},
 * {@code ShaftMcpInvocationServiceCacheTest#fakeProject}). {@code
 * ApplicationManager.setApplication(Application, Disposable)} is public platform API: it captures
 * whatever application was installed before (here, {@code null}) and registers a Disposer hook
 * that restores it once the given {@link Disposable} is disposed -- confirmed by decompiling
 * {@code ApplicationManager.class} (the two-arg overload registers a
 * {@code Disposer.register(parentDisposable, () -> ourApplication = previous)} callback before
 * swapping in the new instance). So installation here is fully reversible per test and never
 * leaks a fake {@code Application} into any other test class sharing the Gradle test JVM.</p>
 */
final class LiveChatToolE2ESupport implements AutoCloseable {
    private static final Duration POLL_INTERVAL = Duration.ofMillis(150);

    private final Disposable applicationDisposable;
    private final ShaftMcpInvocationService invocationService;
    private final ShaftPluginExecutor pluginExecutor;
    private final ToolApprovalService approvalService;
    /**
     * Incremented, from {@link #fakeApplication}'s {@code invokeLater} handler, AFTER each
     * {@code invokeLater} runnable finishes running on the completing (background executor) thread.
     * {@code dispatchApprovedTool}'s completion callback sets the panel's private {@code running}
     * flag to {@code false} (early in {@code showResult}) and its private {@code lastRawResponse}
     * field (later in the same {@code showResult} call) on that SAME thread, in that order -- but
     * both are plain, non-volatile fields, so a separate polling thread reading them via reflection
     * has no Java Memory Model guarantee it ever observes the second write, or observes it in order,
     * without an explicit happens-before edge. Reading this {@link AtomicLong} (a volatile read) after
     * observing it advance past a value captured immediately before {@link #send}'s {@code doClick()}
     * establishes exactly that edge: every plain write the worker thread made before incrementing this
     * counter -- including {@code lastRawResponse} -- is guaranteed visible once the increment is
     * observed. Discovered via genuine intermittent flakiness (different, otherwise-passing tool calls
     * failed with "expected a non-blank response" across repeated full-suite runs) before this fix.
     */
    private final AtomicLong invokeLaterGeneration;
    final Project project;

    private LiveChatToolE2ESupport(
            Disposable applicationDisposable,
            ShaftMcpInvocationService invocationService,
            ShaftPluginExecutor pluginExecutor,
            ToolApprovalService approvalService,
            Project project,
            AtomicLong invokeLaterGeneration) {
        this.applicationDisposable = applicationDisposable;
        this.invocationService = invocationService;
        this.pluginExecutor = pluginExecutor;
        this.approvalService = approvalService;
        this.project = project;
        this.invokeLaterGeneration = invokeLaterGeneration;
    }

    /**
     * Installs a fake {@link Application} (application-scope services: {@link ShaftSettingsState},
     * {@link ShaftPluginExecutor}) and builds a fake {@link Project} backed by ONE real, live {@link
     * ShaftMcpInvocationService} (so a stateful chain like {@code capture_start} -> {@code
     * capture_stop} shares the same spawned MCP server process across calls in a test) and ONE real
     * {@link ToolApprovalService}, pre-armed with {@code APPROVE_ALL_TOOLS} so {@code gateTool} never
     * blocks on an interactive approval widget this headless test cannot click.
     *
     * @param workspace   working directory the spawned MCP server runs in (becomes {@code
     *                    project.getBasePath()})
     * @param mcpCommand  the exact {@code mcpCommand} settings string (e.g. {@code "\"java\"
     *                    \"@shaft-mcp-live.args\""}), parsed by {@code ShaftMcpInvocationService}
     *                    exactly as production code parses a user-configured command
     * @return the installed fixture; MUST be {@link #close()}d to restore the real (null)
     *         application and shut down the spawned MCP process
     */
    static LiveChatToolE2ESupport install(Path workspace, String mcpCommand) {
        Disposable applicationDisposable = Disposer.newDisposable("live-chat-tool-e2e");
        ShaftPluginExecutor pluginExecutor = new ShaftPluginExecutor();
        ShaftSettingsState settingsState = new ShaftSettingsState();
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = mcpCommand;
        settings.mcpSetupComplete = true;
        settingsState.loadState(settings);

        AtomicLong invokeLaterGeneration = new AtomicLong();
        Application fakeApplication = fakeApplication(settingsState, pluginExecutor, invokeLaterGeneration);
        ApplicationManager.setApplication(fakeApplication, applicationDisposable);

        ToolApprovalService approvalService = new ToolApprovalService();
        approvalService.record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "");

        Project project = fakeProject(workspace, approvalService);
        // Built AFTER ApplicationManager.setApplication so the real constructor (which touches no
        // platform API itself) sees a consistent world; ShaftMcpInvocationService.getInstance(project)
        // in dispatchApprovedTool resolves to exactly this memoized instance via the project's own
        // getService stub below.
        ShaftMcpInvocationService invocationService = new ShaftMcpInvocationService(project);
        INVOCATION_SERVICES.put(project, invocationService);

        return new LiveChatToolE2ESupport(
                applicationDisposable, invocationService, pluginExecutor, approvalService, project, invokeLaterGeneration);
    }

    /** Builds a chat panel wired to this fixture's fake project, ready to {@link #send}. */
    ShaftAssistantPanel newPanel() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(project, ShaftSettingsState.getInstance().getState(),
                ShaftAssistantChatState.getInstance(null));
        // Construction fires warmToolsCache()'s fire-and-forget startListTools() call, which acquires
        // the shared MCP client/spawns the server process on first use -- the exact same acquisition
        // path this fixture's first real send() also triggers. Reproduced empirically: without this
        // grace period, the very first send() after newPanel() intermittently read back a blank
        // lastRawResponse (a CancellationException path -- see send()'s javadoc for how a cancelled
        // call reports "" -- most likely from the two racing for the same not-yet-spawned client).
        // A short, fixed settle avoids the race without masking any assertion in a retry loop.
        try {
            Thread.sleep(500);
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
        }
        return panel;
    }

    /**
     * Types {@code prompt} into the real composer and clicks the real "Send" button (accessible name
     * {@code "Send assistant prompt"}), exactly as a user would, then blocks until the panel's
     * private {@code running} flag drops back to {@code false} (verified against
     * {@code ShaftAssistantPanel} source: the only writer of {@code running} is {@code setRunning},
     * called for THIS dispatch at the top of {@code dispatchApprovedTool} (true) and inside {@code
     * showResult} (false) once the real tool call resolves; the local-model-refresh path (@{code
     * refreshLocalModelsIfNeeded}/{@code applyLocalModels}, also kicked off from {@code
     * setRunning(true, ...)}) never touches {@code running} or {@code lastRawResponse}, confirmed by
     * reading both methods in full).
     *
     * <p>An earlier version of this method compared {@code lastRawResponse} against its pre-call
     * value instead of checking {@code running}, to dodge a theorized (but unverified) race with the
     * model-refresh path; that heuristic broke instead on two DIFFERENT real tool calls that
     * legitimately return byte-identical text (e.g. two void-returning tools both rendering as
     * {@code "Done"}, or two calls hitting the same generic failure message) -- reproduced
     * empirically across repeated full-suite runs. Checking {@code running} has no such blind spot.
     *
     * <p>A further, rarer race (reproduced roughly 1-in-15 across many chained multi-call tests):
     * the Send button's own {@code actionListener} does {@code if (running) cancelOrKillCurrent();
     * else send(project);} -- so if {@code running} is still momentarily {@code true} the instant
     * {@code doClick()} runs (the tail end of the PREVIOUS call's completion racing this call's
     * click on separate threads), the click is swallowed as a no-op cancel instead of starting a new
     * dispatch, and nothing ever appends the prompt or advances {@link #invokeLaterGeneration} --
     * {@link #awaitSettled} then times out waiting for a completion that was never dispatched.
     * {@code send(project)}'s first action is always {@code append("user", text, "")}, so checking
     * the transcript for the just-typed prompt right after {@code doClick()} distinguishes "genuinely
     * dispatched" from "swallowed as cancel" with no ambiguity; a bounded single retry click covers
     * the swallowed case without masking a real failure (it never retries the ASSERTION, only the
     * click, and only once).
     *
     * @return the panel's raw tool response text ({@code lastRawResponse}) after the call settles
     */
    String send(ShaftAssistantPanel panel, String prompt, Duration timeout) {
        panel.prefillPrompt(prompt);
        JButton sendButton = findButton(panel, "Send assistant prompt");
        assertNotNull(sendButton, "Missing Send button");
        long generationBeforeSend = invokeLaterGeneration.get();
        sendButton.doClick();
        if (!panel.transcriptMarkdown().contains(prompt)) {
            // Swallowed as a cancel (see javadoc above): the composer still holds the prompt (send()
            // clears it only on a genuine dispatch), so a second click is a genuine retry, not a
            // duplicate submission.
            sendButton.doClick();
        }
        awaitSettled(panel, timeout, generationBeforeSend);
        String response = rawResponse(panel);
        recordExample(prompt, response);
        return response;
    }

    /**
     * Records the exact {@code {request, response}} pair this call produced against a REAL MCP
     * server -- issue #3872/#3866 T6's "example request/response recorded per-skill" deliverable --
     * into {@code build/live-tool-e2e-examples/<toolName>.json}, keyed off the {@code /mcp
     * <toolName> [json]} slash command every test in this suite uses (see the class docs). A later,
     * separate step folds these into {@code tool-index.json}'s {@code example} fields and each
     * skill's {@code ## Example calls} section; recording here (rather than scraping console/report
     * output) is deliberate so the example is exactly what a real live call produced, never
     * hand-transcribed. Silently a no-op for any prompt not using {@code /mcp} (none in this suite).
     */
    private static void recordExample(String prompt, String rawResponse) {
        String trimmed = prompt.trim();
        if (!trimmed.startsWith("/mcp ")) {
            return;
        }
        String rest = trimmed.substring("/mcp ".length()).trim();
        int firstSpace = rest.indexOf(' ');
        String toolName = firstSpace < 0 ? rest : rest.substring(0, firstSpace);
        String requestJson = firstSpace < 0 ? "{}" : rest.substring(firstSpace + 1).trim();
        if (toolName.isBlank()) {
            return;
        }
        try {
            Path examplesDir = Path.of("build", "live-tool-e2e-examples").toAbsolutePath().normalize();
            Files.createDirectories(examplesDir);
            JsonObject record = new JsonObject();
            record.addProperty("tool", toolName);
            try {
                record.add("request", JsonParser.parseString(requestJson.isBlank() ? "{}" : requestJson));
            } catch (RuntimeException malformedRequest) {
                record.addProperty("request", requestJson);
            }
            record.addProperty("rawResponse", rawResponse);
            record.addProperty("responsePayload", unwrapToolPayload(rawResponse));
            Files.writeString(examplesDir.resolve(toolName + ".json"), record.toString(),
                    java.nio.charset.StandardCharsets.UTF_8);
        } catch (java.io.IOException recordingFailure) {
            // Recording is best-effort evidence capture, never part of the behavior under test.
        }
    }

    private void awaitSettled(ShaftAssistantPanel panel, Duration timeout, long generationBeforeSend) {
        Instant deadline = Instant.now().plus(timeout);
        int stableChecks = 0;
        while (Instant.now().isBefore(deadline)) {
            // The generation check (a volatile AtomicLong read) MUST come before isRunning: it is the
            // happens-before edge that makes trusting that plain-field read safe -- see the
            // invokeLaterGeneration field doc. A blank rawResponse is treated as NOT settled even when
            // generation/running look done: it is the exact value showCancelledToolResult writes
            // (showResponse(..., "")), and a rare cross-call race (reproduced empirically across many
            // full-suite runs, roughly 1 in 15) can let a NEW send() observe a stale "settled" state
            // from the PREVIOUS call before that call's own completion has fully landed. Requiring two
            // consecutive stable observations (this loop's debounce) additionally guards against a
            // single transient flicker in that race.
            boolean looksSettled = invokeLaterGeneration.get() > generationBeforeSend
                    && !isRunning(panel)
                    && !rawResponse(panel).isBlank();
            if (looksSettled) {
                stableChecks++;
                if (stableChecks >= 2) {
                    return;
                }
            } else {
                stableChecks = 0;
            }
            sleep();
        }
        fail("Assistant panel never settled (still running, or response stayed blank) within " + timeout
                + ". Transcript so far:\n" + panel.transcriptMarkdown());
    }

    private static boolean isRunning(ShaftAssistantPanel panel) {
        return (boolean) readField(panel, "running");
    }

    private static void sleep() {
        try {
            Thread.sleep(POLL_INTERVAL.toMillis());
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            fail("Interrupted while awaiting assistant response");
        }
    }

    private static String rawResponse(ShaftAssistantPanel panel) {
        return (String) readField(panel, "lastRawResponse");
    }

    /**
     * Unwraps the panel's raw MCP JSON-RPC {@code CallToolResult} envelope (e.g. {@code
     * {"content":[{"type":"text","text":"<payload>"}],"isError":false}}, exactly what {@code
     * ShaftMcpInvocationService.toolResult} stores via {@code result.toString()}) down to the actual
     * tool payload string -- the shape every hand-authored {@code ## Example calls} response in
     * shaft-skills already uses, so recorded examples match without extra reformatting.
     *
     * @param rawResponse the panel's {@code lastRawResponse} after a settled {@link #send}
     * @return the unwrapped tool payload text, or {@code rawResponse} unchanged if it is not a
     *         recognizable {@code content[0].text} envelope (e.g. already-plain text)
     */
    static String unwrapToolPayload(String rawResponse) {
        if (rawResponse == null || rawResponse.isBlank()) {
            return rawResponse;
        }
        try {
            JsonElement parsed = JsonParser.parseString(rawResponse);
            if (!parsed.isJsonObject()) {
                return rawResponse;
            }
            JsonObject envelope = parsed.getAsJsonObject();
            if (!envelope.has("content") || !envelope.get("content").isJsonArray()
                    || envelope.getAsJsonArray("content").isEmpty()) {
                return rawResponse;
            }
            JsonElement first = envelope.getAsJsonArray("content").get(0);
            if (!first.isJsonObject() || !first.getAsJsonObject().has("text")) {
                return rawResponse;
            }
            return first.getAsJsonObject().get("text").getAsString();
        } catch (RuntimeException malformed) {
            return rawResponse;
        }
    }

    private static Object readField(ShaftAssistantPanel panel, String name) {
        try {
            Field field = ShaftAssistantPanel.class.getDeclaredField(name);
            field.setAccessible(true);
            return field.get(panel);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Cannot read ShaftAssistantPanel#" + name, exception);
        }
    }

    private static JButton findButton(Component component, String accessibleName) {
        if (component instanceof JButton button && component instanceof JComponent jComponent) {
            AccessibleContext context = jComponent.getAccessibleContext();
            if (context != null && accessibleName.equals(context.getAccessibleName())) {
                return button;
            }
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JButton found = findButton(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    /**
     * Restores the real (previously {@code null}) {@link Application} via {@link Disposer#dispose}
     * and disposes the real {@link ShaftMcpInvocationService}/{@link ShaftPluginExecutor} instances
     * so the spawned MCP server process and worker thread pool are shut down -- never left as
     * orphaned processes/threads after the test.
     */
    @Override
    public void close() {
        invocationService.dispose();
        pluginExecutor.dispose();
        Disposer.dispose(applicationDisposable);
    }

    /**
     * Keyed by the exact {@link Project} instance so the fake project's {@code getService} stub
     * (a plain {@link Proxy}, which cannot hold typed fields of its own) can hand back the one real
     * {@link ShaftMcpInvocationService} built for it in {@link #install}.
     */
    private static final Map<Project, ShaftMcpInvocationService> INVOCATION_SERVICES = new ConcurrentHashMap<>();

    private static Project fakeProject(Path workspace, ToolApprovalService approvalService) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath", "getBasePathString":
                            return workspace.toString();
                        case "getName":
                            return "shaft-live-tool-e2e-project";
                        case "isDisposed":
                            return false;
                        case "getService":
                            Class<?> serviceClass = (Class<?>) arguments[0];
                            if (serviceClass == ShaftMcpInvocationService.class) {
                                return INVOCATION_SERVICES.get(proxy);
                            }
                            if (serviceClass == ToolApprovalService.class) {
                                return approvalService;
                            }
                            if (serviceClass == FileEditorManager.class) {
                                // send()'s openFileContext(project) unconditionally dereferences this once
                                // project != null (ShaftAssistantPanel.java:3919-3920); a no-open-files fake
                                // is the honest state for a headless test with no real editor.
                                return NO_OPEN_FILES_EDITOR_MANAGER;
                            }
                            return null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    /**
     * {@code getSelectedTextEditor() -> null} short-circuits {@code openFileContext} to "no open
     * file" -- the only method of the 27 this abstract class declares that {@code
     * ShaftAssistantPanel#openFileContext} (the only caller on the {@code send()} path) actually
     * invokes; {@link FileEditorManager} is an abstract CLASS (not an interface), so a {@link Proxy}
     * cannot stand in for it the way {@link #fakeApplication}/{@link #fakeProject} do.
     */
    private static final class NoOpFileEditorManager extends FileEditorManager {
        @Override
        public FileEditorComposite getComposite(VirtualFile file) {
            return null;
        }

        @Override
        public FileEditor[] openFile(VirtualFile file, boolean focusEditor) {
            return new FileEditor[0];
        }

        @Override
        public List<FileEditor> openFile(VirtualFile file) {
            return List.of();
        }

        @Override
        public void closeFile(VirtualFile file) {
            // no-op: no real editors are ever open in this headless fixture
        }

        @Override
        public Editor openTextEditor(OpenFileDescriptor descriptor, boolean focusEditor) {
            return null;
        }

        @Override
        public Editor getSelectedTextEditor() {
            return null;
        }

        @Override
        public boolean isFileOpen(VirtualFile file) {
            return false;
        }

        @Override
        public VirtualFile[] getOpenFiles() {
            return new VirtualFile[0];
        }

        @Override
        public List<VirtualFile> getOpenFilesWithRemotes() {
            return List.of();
        }

        @Override
        public VirtualFile[] getSelectedFiles() {
            return new VirtualFile[0];
        }

        @Override
        public FileEditor[] getSelectedEditors() {
            return new FileEditor[0];
        }

        @Override
        public FileEditor getSelectedEditor(VirtualFile file) {
            return null;
        }

        @Override
        public FileEditor[] getEditors(VirtualFile file) {
            return new FileEditor[0];
        }

        @Override
        public FileEditor[] getAllEditors(VirtualFile file) {
            return new FileEditor[0];
        }

        @Override
        public List<FileEditor> getAllEditorList(VirtualFile file) {
            return List.of();
        }

        @Override
        public FileEditor[] getAllEditors() {
            return new FileEditor[0];
        }

        @Override
        public void addTopComponent(FileEditor editor, JComponent component) {
            // no-op
        }

        @Override
        public void removeTopComponent(FileEditor editor, JComponent component) {
            // no-op
        }

        @Override
        public void addBottomComponent(FileEditor editor, JComponent component) {
            // no-op
        }

        @Override
        public void removeBottomComponent(FileEditor editor, JComponent component) {
            // no-op
        }

        @Override
        public List<FileEditor> openFileEditor(FileEditorNavigatable descriptor, boolean focusEditor) {
            return List.of();
        }

        @Override
        public Project getProject() {
            return null;
        }

        @Override
        @SuppressWarnings("removal")
        public void registerExtraEditorDataProvider(EditorDataProvider provider, Disposable parentDisposable) {
            // no-op
        }

        @Override
        @SuppressWarnings("removal")
        public Object getData(String dataId, Editor editor, Caret caret) {
            return null;
        }

        @Override
        public void setSelectedEditor(VirtualFile file, String fileEditorProviderId) {
            // no-op
        }

        @Override
        public void runWhenLoaded(Editor editor, Runnable runnable) {
            // no-op: matches "never loaded" -- runnable is simply never invoked
        }
    }

    private static final FileEditorManager NO_OPEN_FILES_EDITOR_MANAGER = new NoOpFileEditorManager();

    private static Application fakeApplication(ShaftSettingsState settingsState, ShaftPluginExecutor pluginExecutor,
            AtomicLong invokeLaterGeneration) {
        return (Application) Proxy.newProxyInstance(Application.class.getClassLoader(), new Class<?>[]{Application.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getService":
                            Class<?> serviceClass = (Class<?>) arguments[0];
                            if (serviceClass == ShaftSettingsState.class) {
                                return settingsState;
                            }
                            if (serviceClass == ShaftPluginExecutor.class) {
                                return pluginExecutor;
                            }
                            return null;
                        case "isUnitTestMode", "isHeadlessEnvironment", "isCommandLine":
                            return true;
                        case "isDisposed":
                            return false;
                        case "isReadAccessAllowed", "isDispatchThread", "isWriteIntentLockAcquired":
                            return true;
                        case "invokeLater", "invokeLaterOnWriteThread":
                            // No real EDT exists in this Gradle test JVM (issue #3872/#3866 T6): running the
                            // callback inline on the calling (ShaftPluginExecutor worker) thread is the same
                            // trade-off ShaftAssistantPanel#runOnEdt already makes for its
                            // ApplicationManager-unavailable fallback branch. The generation bump AFTER
                            // running the callback is the memory-visibility fence awaitSettled relies on --
                            // see the invokeLaterGeneration field doc.
                            ((Runnable) arguments[0]).run();
                            invokeLaterGeneration.incrementAndGet();
                            return null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive()) {
            return null;
        }
        if (returnType == boolean.class) {
            return false;
        }
        if (returnType == void.class) {
            return null;
        }
        return 0;
    }
}
