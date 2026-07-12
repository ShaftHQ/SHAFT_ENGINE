package com.shaft.intellij.actions;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.shaft.intellij.java.JavaTargetContextResolver;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.project.ShaftProjectDetector;
import org.jetbrains.annotations.NotNull;

/**
 * Picks the element most recently inspected in a live SHAFT Capture session and inserts a
 * copy-paste {@code SHAFT.GUI.Locator...} snippet at the editor caret via {@code
 * capture_pick_locator} (mirrors Playwright's "Pick Locator").
 *
 * <p>{@code capture_pick_locator} ranks locator candidates the caller supplies; this action has no
 * direct DOM access to derive them itself (only the recorder overlay running inside the managed
 * browser does), so it invokes the tool with an empty candidate set as a live readiness probe: a
 * blank {@code snippet} in an otherwise successful response means nothing has been picked yet in
 * the browser, which is reported identically to "no active capture session" since either way there
 * is nothing to insert.</p>
 */
public final class PickLocatorAtCaretAction extends AnAction implements DumbAware {
    static final String NO_PICK_MESSAGE = "No locator has been picked yet. Start a SHAFT Capture "
            + "session, switch the recorder to inspect mode, and click the target element in the "
            + "managed browser, then try again.";
    static final String READ_ONLY_MESSAGE = "The current file could not be made writable.";
    private static final String TOOL_NAME = "capture_pick_locator";
    private static final String COMMAND_NAME = "Insert SHAFT Locator";

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (project == null || editor == null || !isAvailable(project, editor)) {
            return;
        }
        ShaftMcpInvocationService.getInstance(project)
                .startTool(TOOL_NAME, new JsonObject())
                .future()
                .whenComplete((result, error) -> ApplicationManager.getApplication()
                        .invokeLater(() -> handleResult(project, editor, result, error)));
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        event.getPresentation().setEnabledAndVisible(
                project != null && editor != null && isAvailable(project, editor));
    }

    private static boolean isAvailable(Project project, Editor editor) {
        if (!ShaftProjectDetector.isShaftProject(project)) {
            return false;
        }
        PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument());
        return JavaTargetContextResolver.resolve(file, editor.getCaretModel().getOffset()) != null;
    }

    private static void handleResult(Project project, Editor editor, ShaftMcpToolResult result, Throwable error) {
        if (error != null) {
            ShaftNotifier.warn(project, "SHAFT", "Pick Locator failed: " + error.getMessage());
            return;
        }
        PickOutcome outcome = classify(result);
        switch (outcome.kind()) {
            case TOOL_FAILURE -> ShaftNotifier.warn(project, "SHAFT", "Pick Locator failed: " + outcome.detail());
            case NO_PICK -> ShaftNotifier.warn(project, "SHAFT", NO_PICK_MESSAGE);
            case SNIPPET -> insertSnippet(project, editor, outcome.detail());
            default -> ShaftNotifier.warn(project, "SHAFT", "Pick Locator returned an unexpected outcome: " + outcome.kind());
        }
    }

    private static void insertSnippet(Project project, Editor editor, String snippet) {
        Document document = editor.getDocument();
        if (!FileDocumentManager.getInstance().requestWriting(document, project)) {
            ShaftNotifier.warn(project, "SHAFT", READ_ONLY_MESSAGE);
            return;
        }
        int offset = resolveInsertionOffset(editor.getCaretModel().getOffset(), document.getTextLength());
        WriteCommandAction.writeCommandAction(project)
                .withName(COMMAND_NAME)
                .run(() -> document.insertString(offset, snippet));
    }

    /**
     * Clamps a caret offset captured before an async MCP round trip to the document's current
     * bounds, since the document may have changed size while the request was in flight.
     *
     * @param caretOffset caret offset to clamp
     * @param documentLength current document length
     * @return an offset guaranteed to be within {@code [0, documentLength]}
     */
    static int resolveInsertionOffset(int caretOffset, int documentLength) {
        return Math.max(0, Math.min(caretOffset, documentLength));
    }

    /**
     * Classifies a completed {@code capture_pick_locator} invocation into an outcome the caller can
     * act on without re-inspecting {@link ShaftMcpToolResult} internals.
     *
     * @param result completed tool result, or {@code null} if none was produced
     * @return classified outcome
     */
    static PickOutcome classify(ShaftMcpToolResult result) {
        if (result == null || !result.success()) {
            String detail = result == null ? "No response from SHAFT MCP." : result.output();
            return new PickOutcome(PickOutcomeKind.TOOL_FAILURE, detail);
        }
        String snippet = extractSnippet(result.output());
        return snippet == null || snippet.isBlank()
                ? new PickOutcome(PickOutcomeKind.NO_PICK, "")
                : new PickOutcome(PickOutcomeKind.SNIPPET, snippet);
    }

    /**
     * Extracts the {@code snippet} field from a {@code capture_pick_locator} tool result, unwrapping
     * the MCP {@code content[].text} envelope when the payload is wrapped in one.
     *
     * @param output raw tool output text
     * @return the snippet text, or {@code null} when it could not be found
     */
    static String extractSnippet(String output) {
        JsonObject payload = jsonObject(output);
        if (payload == null) {
            return null;
        }
        String direct = stringField(payload, "snippet");
        if (direct != null) {
            return direct;
        }
        JsonElement content = payload.get("content");
        if (content == null || !content.isJsonArray()) {
            return null;
        }
        for (JsonElement entry : content.getAsJsonArray()) {
            String nestedSnippet = snippetFromContentEntry(entry);
            if (nestedSnippet != null) {
                return nestedSnippet;
            }
        }
        return null;
    }

    private static String snippetFromContentEntry(JsonElement entry) {
        if (!entry.isJsonObject()) {
            return null;
        }
        JsonElement text = entry.getAsJsonObject().get("text");
        if (text == null || !text.isJsonPrimitive()) {
            return null;
        }
        JsonObject nested = jsonObject(text.getAsString());
        return nested == null ? null : stringField(nested, "snippet");
    }

    private static JsonObject jsonObject(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        try {
            JsonElement parsed = JsonParser.parseString(text);
            return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (RuntimeException malformed) {
            return null;
        }
    }

    private static String stringField(JsonObject object, String field) {
        JsonElement value = object.get(field);
        return value != null && value.isJsonPrimitive() ? value.getAsString() : null;
    }

    /** Classification of a completed pick-locator invocation. */
    enum PickOutcomeKind {
        SNIPPET,
        NO_PICK,
        TOOL_FAILURE
    }

    /**
     * Classified pick-locator outcome.
     *
     * @param kind outcome kind
     * @param detail the snippet for {@link PickOutcomeKind#SNIPPET}, the diagnostic text for
     *               {@link PickOutcomeKind#TOOL_FAILURE}, or blank for {@link PickOutcomeKind#NO_PICK}
     */
    record PickOutcome(PickOutcomeKind kind, String detail) {
    }
}
