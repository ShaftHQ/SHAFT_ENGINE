package com.shaft.intellij.ui;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.editor.Caret;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.EditorDataProvider;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorComposite;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorNavigatable;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;

import javax.swing.JComponent;
import java.util.List;

/**
 * {@code getSelectedTextEditor() -> null} short-circuits {@code
 * ShaftAssistantPanel#openFileContext} to "no open file" -- the only method of the 27 this
 * abstract class declares that {@code openFileContext} (the only caller on the {@code send()}
 * path) actually invokes; {@link FileEditorManager} is an abstract CLASS (not an interface), so a
 * {@link java.lang.reflect.Proxy} cannot stand in for it the way a fake {@code Project} does.
 *
 * <p>Extracted from {@link LiveChatToolE2ESupport} (originally private there) so any test that
 * builds a fake {@link Project} for {@code send()} -- not just the live-tool-E2E fixture -- can
 * satisfy {@code project.getService(FileEditorManager.class)} without re-declaring all 27
 * overrides (issue #3988).
 */
final class NoOpFileEditorManager extends FileEditorManager {
    static final FileEditorManager INSTANCE = new NoOpFileEditorManager();

    private NoOpFileEditorManager() {
    }

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
