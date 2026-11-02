package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JLabel;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Plain-logic coverage for {@link VisualBaselinesPanel}: row discovery from a temp-directory
 * fixture, and the Accept/Reject filesystem operations. No IntelliJ platform fixtures are
 * needed since discovery and the accept/reject actions are pure {@link java.nio.file.Path}
 * operations.
 */
class VisualBaselinesPanelTest {

    @Test
    void discoverBaselineRowsFindsOnlyComparisonsWithAPendingDiff(@TempDir Path directory) throws IOException {
        writeImage(directory.resolve("signInHeader_chrome_windows.png"));
        writeImage(directory.resolve("signInHeader_chrome_windows_diff.png"));
        writeImage(directory.resolve("cartBadge_chrome_windows.png"));
        // No matching diff for cartBadge: it passed its last run and should not be listed.

        List<VisualBaselinesPanel.BaselineRow> rows = VisualBaselinesPanel.discoverBaselineRows(directory);

        assertAll(
                () -> assertEquals(1, rows.size()),
                () -> assertEquals("signInHeader_chrome_windows.png", rows.get(0).name()),
                () -> assertEquals(directory.resolve("signInHeader_chrome_windows.png"), rows.get(0).baselinePath()),
                () -> assertEquals(directory.resolve("signInHeader_chrome_windows_diff.png"), rows.get(0).diffPath()));
    }

    @Test
    void discoverBaselineRowsWalksNestedPlatformAndBrowserSubfolders(@TempDir Path directory) throws IOException {
        Path nested = directory.resolve("Windows").resolve("chrome");
        Files.createDirectories(nested);
        writeImage(nested.resolve("header_chrome_windows.png"));
        writeImage(nested.resolve("header_chrome_windows_diff.png"));

        List<VisualBaselinesPanel.BaselineRow> rows = VisualBaselinesPanel.discoverBaselineRows(directory);

        assertAll(
                () -> assertEquals(1, rows.size()),
                () -> assertEquals(Path.of("Windows", "chrome", "header_chrome_windows.png").toString(),
                        rows.get(0).name()));
    }

    @Test
    void discoverBaselineRowsReturnsEmptyListWhenNoDiffsExist(@TempDir Path directory) throws IOException {
        writeImage(directory.resolve("cartBadge_chrome_windows.png"));

        List<VisualBaselinesPanel.BaselineRow> rows = VisualBaselinesPanel.discoverBaselineRows(directory);

        assertTrue(rows.isEmpty());
    }

    @Test
    void acceptBaselineDeletesBothTheBaselineAndTheDiffMarker(@TempDir Path directory) throws IOException {
        Path baseline = directory.resolve("signInHeader_chrome_windows.png");
        Path diff = directory.resolve("signInHeader_chrome_windows_diff.png");
        writeImage(baseline);
        writeImage(diff);
        VisualBaselinesPanel.BaselineRow row =
                new VisualBaselinesPanel.BaselineRow("signInHeader_chrome_windows.png", baseline, diff);

        VisualBaselinesPanel.acceptBaseline(row);

        assertAll(
                () -> assertFalse(Files.exists(baseline), "accept should remove the outdated baseline"),
                () -> assertFalse(Files.exists(diff), "accept should remove the stale diff marker"));
    }

    @Test
    void rejectBaselineKeepsTheBaselineAndOnlyDeletesTheDiffMarker(@TempDir Path directory) throws IOException {
        Path baseline = directory.resolve("signInHeader_chrome_windows.png");
        Path diff = directory.resolve("signInHeader_chrome_windows_diff.png");
        writeImage(baseline);
        writeImage(diff);
        VisualBaselinesPanel.BaselineRow row =
                new VisualBaselinesPanel.BaselineRow("signInHeader_chrome_windows.png", baseline, diff);

        VisualBaselinesPanel.rejectBaseline(row);

        assertAll(
                () -> assertTrue(Files.exists(baseline), "reject should keep the current baseline"),
                () -> assertFalse(Files.exists(diff), "reject should remove the stale diff marker"));
    }

    @Test
    void acceptAndRejectToleratePreviouslyDeletedFiles(@TempDir Path directory) {
        Path baseline = directory.resolve("missing_chrome_windows.png");
        Path diff = directory.resolve("missing_chrome_windows_diff.png");
        VisualBaselinesPanel.BaselineRow row =
                new VisualBaselinesPanel.BaselineRow("missing_chrome_windows.png", baseline, diff);

        assertAll(
                () -> VisualBaselinesPanel.acceptBaseline(row),
                () -> VisualBaselinesPanel.rejectBaseline(row));
    }

    @Test
    void scanButtonPopulatesRowsAndSelectingARowEnablesTheActionButtons(@TempDir Path directory) throws IOException {
        writeImage(directory.resolve("signInHeader_chrome_windows.png"));
        writeImage(directory.resolve("signInHeader_chrome_windows_diff.png"));
        VisualBaselinesPanel panel = new VisualBaselinesPanel(null);
        panel.directoryFieldForTest().setText(directory.toString());
        JButton acceptButton = panel.acceptButtonForTest();
        JButton rejectButton = panel.rejectButtonForTest();
        assertFalse(acceptButton.isEnabled(), "no row selected yet");
        assertFalse(rejectButton.isEnabled(), "no row selected yet");

        panel.scanButtonForTest().doClick();

        assertEquals(1, panel.rowListForTest().getModel().getSize());
        panel.rowListForTest().setSelectedIndex(0);
        assertAll(
                () -> assertTrue(acceptButton.isEnabled(), "selecting a row should enable Accept"),
                () -> assertTrue(rejectButton.isEnabled(), "selecting a row should enable Reject"));
    }

    @Test
    void scanReportsAMissingDirectoryInTheStatusLabelInsteadOfThrowing(@TempDir Path directory) {
        VisualBaselinesPanel panel = new VisualBaselinesPanel(null);
        panel.directoryFieldForTest().setText(directory.resolve("does-not-exist").toString());

        panel.scanButtonForTest().doClick();

        JLabel status = panel.statusLabelForTest();
        assertAll(
                () -> assertNotNull(status.getText()),
                () -> assertTrue(status.getText().contains("not found")),
                () -> assertEquals(0, panel.rowListForTest().getModel().getSize()));
    }

    private static void writeImage(Path path) throws IOException {
        BufferedImage image = new BufferedImage(4, 4, BufferedImage.TYPE_INT_RGB);
        if (!ImageIO.write(image, "png", path.toFile())) {
            throw new IOException("No PNG writer available for " + path);
        }
    }
}
