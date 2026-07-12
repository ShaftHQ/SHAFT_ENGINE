package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBList;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;

import javax.imageio.ImageIO;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.Icon;
import javax.swing.SwingConstants;
import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

/**
 * Lists {@code matchesScreenshot()} visual baselines that mismatched on their last run (a
 * {@code <hash>_<browser>_<platform>_diff.png} sits next to the baseline; see
 * {@code ImageProcessingActions#compareScreenshotAgainstBaselineByHash}) and lets a developer
 * Accept or Reject each one.
 * <p>
 * There is no recorded "actual" screenshot on disk (only the baseline and the diff), so v1
 * Accept semantics are: delete the outdated baseline and its diff marker so the next test run
 * records a fresh baseline. Reject only removes the diff marker and keeps the current baseline.
 */
final class VisualBaselinesPanel extends JPanel {
    /** Mirrors {@code Paths.dynamicObjectRepositoryPath}'s {@code @DefaultValue}. */
    private static final String DEFAULT_BASELINE_DIRECTORY = "src/main/resources/dynamicObjectRepository";
    private static final String DIFF_SUFFIX = "_diff.png";
    private static final int MAX_PREVIEW_DIMENSION = 220;

    private final Project project;
    private final JBTextField directoryField;
    private final DefaultListModel<BaselineRow> listModel = new DefaultListModel<>();
    private final JBList<BaselineRow> rowList = new JBList<>(listModel);
    private final JLabel baselinePreview = previewLabel();
    private final JLabel diffPreview = previewLabel();
    private final JLabel statusLabel = new JLabel(" ");
    private final JButton scanButton;
    private final JButton acceptButton;
    private final JButton rejectButton;

    VisualBaselinesPanel(Project project) {
        super(new BorderLayout(8, 8));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));

        directoryField = field("Baseline directory", DEFAULT_BASELINE_DIRECTORY);

        JPanel fields = new JPanel(new GridBagLayout());
        addFieldRow(fields, 0, row("Directory", 'D', directoryField));

        scanButton = button("Scan", "List baseline comparisons with a pending diff from the last mismatched run",
                ShaftIcons.SEARCH, this::scan);
        JPanel fieldsRow = new JPanel(new BorderLayout(6, 0));
        fieldsRow.add(fields, BorderLayout.CENTER);
        fieldsRow.add(scanButton, BorderLayout.EAST);

        JPanel north = new JPanel(new BorderLayout(4, 4));
        north.add(introLabel("Visual Baselines lists matchesScreenshot() comparisons with a pending "
                + "_diff.png from the last mismatched run."), BorderLayout.NORTH);
        north.add(fieldsRow, BorderLayout.SOUTH);

        rowList.getAccessibleContext().setAccessibleName("Visual baseline comparisons");
        rowList.addListSelectionListener(event -> {
            if (!event.getValueIsAdjusting()) {
                onSelectionChanged();
            }
        });
        JBScrollPane listScroll = new JBScrollPane(rowList);
        listScroll.setPreferredSize(JBUI.size(240, 260));

        JPanel previewPanel = new JPanel(new GridLayout(1, 2, 8, 0));
        previewPanel.add(labeledPreview("Baseline", baselinePreview));
        previewPanel.add(labeledPreview("Diff", diffPreview));

        JPanel center = new JPanel(new BorderLayout(8, 8));
        center.add(listScroll, BorderLayout.WEST);
        center.add(previewPanel, BorderLayout.CENTER);

        acceptButton = button("Accept",
                "Removes the stale baseline; the next test run records a new one.", ShaftIcons.CHECK,
                this::acceptSelected);
        rejectButton = button("Reject",
                "Keeps the current baseline; removes only the stale diff marker.", ShaftIcons.CANCEL,
                this::rejectSelected);
        acceptButton.setEnabled(false);
        rejectButton.setEnabled(false);
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(acceptButton);
        actions.add(rejectButton);

        statusLabel.getAccessibleContext().setAccessibleName("Visual baselines status");
        statusLabel.setBorder(JBUI.Borders.emptyTop(4));
        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actions, BorderLayout.NORTH);
        south.add(statusLabel, BorderLayout.SOUTH);

        add(north, BorderLayout.NORTH);
        add(center, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
    }

    // ------------------------------------------------------------------
    // Scan
    // ------------------------------------------------------------------

    private void scan() {
        listModel.clear();
        clearSelection();
        Path directory = resolveDirectory();
        if (!Files.isDirectory(directory)) {
            statusLabel.setText("Baseline directory not found: " + directory);
            return;
        }
        List<BaselineRow> rows;
        try {
            rows = discoverBaselineRows(directory);
        } catch (IOException scanFailure) {
            statusLabel.setText("Could not scan \"" + directory + "\": " + scanFailure.getMessage());
            return;
        }
        rows.forEach(listModel::addElement);
        statusLabel.setText(rows.isEmpty()
                ? "No pending baseline diffs found under " + directory
                : rows.size() + " baseline comparison(s) pending triage.");
    }

    private Path resolveDirectory() {
        String typed = directoryField.getText() == null ? "" : directoryField.getText().trim();
        Path path = Path.of(typed.isBlank() ? DEFAULT_BASELINE_DIRECTORY : typed);
        if (path.isAbsolute() || project == null || project.getBasePath() == null
                || project.getBasePath().isBlank()) {
            return path;
        }
        return Path.of(project.getBasePath()).resolve(path);
    }

    /**
     * Recursively finds pending baseline comparisons (a {@code *_diff.png} next to its baseline)
     * under {@code directory}. Pure filesystem logic with no Swing dependency, so it is directly
     * unit-testable against a temp directory fixture.
     *
     * @param directory baseline root directory to scan
     * @return discovered rows, sorted by their path relative to {@code directory}
     * @throws IOException when the directory cannot be walked
     */
    static List<BaselineRow> discoverBaselineRows(Path directory) throws IOException {
        List<BaselineRow> rows = new ArrayList<>();
        try (Stream<Path> walk = Files.walk(directory)) {
            walk.filter(Files::isRegularFile)
                    .filter(candidate -> candidate.getFileName().toString().endsWith(DIFF_SUFFIX))
                    .forEach(diffPath -> {
                        String diffFileName = diffPath.getFileName().toString();
                        String baselineFileName = diffFileName.substring(0,
                                diffFileName.length() - DIFF_SUFFIX.length()) + ".png";
                        Path baselinePath = diffPath.resolveSibling(baselineFileName);
                        String name = directory.relativize(baselinePath).toString();
                        rows.add(new BaselineRow(name, baselinePath, diffPath));
                    });
        }
        rows.sort(Comparator.comparing(BaselineRow::name));
        return rows;
    }

    /**
     * Accept: deletes the outdated baseline and its diff marker so the next test run records a
     * fresh baseline. There is no recorded "actual" screenshot to promote in place, only the
     * baseline and the diff.
     *
     * @param row the baseline comparison to accept
     * @throws IOException when either file cannot be deleted
     */
    static void acceptBaseline(BaselineRow row) throws IOException {
        Files.deleteIfExists(row.diffPath());
        Files.deleteIfExists(row.baselinePath());
    }

    /**
     * Reject: keeps the current baseline and only removes the stale diff marker.
     *
     * @param row the baseline comparison to reject
     * @throws IOException when the diff file cannot be deleted
     */
    static void rejectBaseline(BaselineRow row) throws IOException {
        Files.deleteIfExists(row.diffPath());
    }

    // ------------------------------------------------------------------
    // Row selection and actions
    // ------------------------------------------------------------------

    private void onSelectionChanged() {
        BaselineRow selected = rowList.getSelectedValue();
        boolean hasSelection = selected != null;
        acceptButton.setEnabled(hasSelection);
        rejectButton.setEnabled(hasSelection);
        if (!hasSelection) {
            clearSelection();
            return;
        }
        setPreview(baselinePreview, selected.baselinePath());
        setPreview(diffPreview, selected.diffPath());
    }

    private void clearSelection() {
        acceptButton.setEnabled(false);
        rejectButton.setEnabled(false);
        resetPreview(baselinePreview);
        resetPreview(diffPreview);
    }

    private void acceptSelected() {
        BaselineRow row = rowList.getSelectedValue();
        if (row == null) {
            return;
        }
        int confirmation = JOptionPane.showConfirmDialog(this,
                "Accept \"" + row.name() + "\"? This removes the stale baseline; the next test run "
                        + "records a new one.",
                "Accept visual baseline", JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
        if (confirmation != JOptionPane.OK_OPTION) {
            return;
        }
        try {
            acceptBaseline(row);
            statusLabel.setText("Accepted \"" + row.name() + "\": baseline removed, next run records a fresh one.");
            listModel.removeElement(row);
        } catch (IOException failure) {
            statusLabel.setText("Could not accept \"" + row.name() + "\": " + failure.getMessage());
        }
    }

    private void rejectSelected() {
        BaselineRow row = rowList.getSelectedValue();
        if (row == null) {
            return;
        }
        try {
            rejectBaseline(row);
            statusLabel.setText("Rejected \"" + row.name() + "\": baseline kept, diff marker removed.");
            listModel.removeElement(row);
        } catch (IOException failure) {
            statusLabel.setText("Could not reject \"" + row.name() + "\": " + failure.getMessage());
        }
    }

    // ------------------------------------------------------------------
    // Image previews (fail-soft: an unreadable image renders a placeholder, never throws)
    // ------------------------------------------------------------------

    private static JLabel previewLabel() {
        JLabel label = new JLabel("Select a row to preview", SwingConstants.CENTER);
        label.setVerticalAlignment(SwingConstants.CENTER);
        label.setBorder(JBUI.Borders.customLine(JBColor.namedColor("Component.borderColor", JBColor.GRAY), 1));
        return label;
    }

    private static JPanel labeledPreview(String title, JLabel imageLabel) {
        JPanel panel = new JPanel(new BorderLayout(4, 4));
        JLabel titleLabel = new JLabel(title);
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD));
        panel.add(titleLabel, BorderLayout.NORTH);
        panel.add(imageLabel, BorderLayout.CENTER);
        return panel;
    }

    private static void resetPreview(JLabel label) {
        label.setIcon(null);
        label.setText("Select a row to preview");
    }

    private static void setPreview(JLabel label, Path imagePath) {
        try {
            BufferedImage image = ImageIO.read(imagePath.toFile());
            if (image == null) {
                label.setIcon(null);
                label.setText("No preview available");
                return;
            }
            label.setIcon(scaledIcon(image));
            label.setText(null);
        } catch (IOException | RuntimeException unreadableImage) {
            label.setIcon(null);
            label.setText("No preview available");
        }
    }

    private static Icon scaledIcon(BufferedImage image) {
        int width = image.getWidth();
        int height = image.getHeight();
        double scale = Math.min(1.0D,
                (double) MAX_PREVIEW_DIMENSION / Math.max(1, Math.max(width, height)));
        int scaledWidth = Math.max(1, (int) Math.round(width * scale));
        int scaledHeight = Math.max(1, (int) Math.round(height * scale));
        Image scaled = image.getScaledInstance(scaledWidth, scaledHeight, Image.SCALE_SMOOTH);
        return new ImageIcon(scaled);
    }

    // ------------------------------------------------------------------
    // Layout helpers (mirrors EvidenceTriagePanel's conventions)
    // ------------------------------------------------------------------

    private static JLabel introLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(label.getFont().deriveFont(Font.BOLD, label.getFont().getSize2D() + 1f));
        label.setBorder(JBUI.Borders.emptyBottom(8));
        label.getAccessibleContext().setAccessibleName(text);
        return label;
    }

    private static JBTextField field(String accessibleName, String value) {
        JBTextField textField = new JBTextField(value);
        textField.getAccessibleContext().setAccessibleName(accessibleName);
        textField.setToolTipText(accessibleName);
        return textField;
    }

    private static JPanel row(String labelText, char mnemonic, JComponent component) {
        JPanel rowPanel = new JPanel(new BorderLayout(4, 4));
        JLabel label = new JLabel(labelText);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(component);
        rowPanel.add(label, BorderLayout.WEST);
        rowPanel.add(component, BorderLayout.CENTER);
        return rowPanel;
    }

    private static void addFieldRow(JPanel panel, int rowIndex, JPanel rowPanel) {
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = rowIndex;
        constraints.weightx = 1.0;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.insets = new Insets(0, 0, 6, 0);
        panel.add(rowPanel, constraints);
    }

    private static JButton button(String text, String description, Icon icon, Runnable action) {
        JButton buttonComponent = new JButton();
        ShaftIconButtons.apply(buttonComponent, description, text, icon);
        buttonComponent.getAccessibleContext().setAccessibleDescription(description);
        buttonComponent.addActionListener(event -> action.run());
        return buttonComponent;
    }

    // ------------------------------------------------------------------
    // Test-only accessors
    // ------------------------------------------------------------------

    JBTextField directoryFieldForTest() {
        return directoryField;
    }

    JButton scanButtonForTest() {
        return scanButton;
    }

    JBList<BaselineRow> rowListForTest() {
        return rowList;
    }

    JLabel statusLabelForTest() {
        return statusLabel;
    }

    JButton acceptButtonForTest() {
        return acceptButton;
    }

    JButton rejectButtonForTest() {
        return rejectButton;
    }

    /**
     * A discovered baseline comparison: a mismatched {@code matchesScreenshot()} run left both the
     * existing baseline and a {@code *_diff.png} marker on disk.
     *
     * @param name display name (baseline path relative to the scanned directory)
     * @param baselinePath on-disk path of the existing baseline image
     * @param diffPath on-disk path of the pixel-diff image written on mismatch
     */
    record BaselineRow(String name, Path baselinePath, Path diffPath) {
        @Override
        public String toString() {
            return name;
        }
    }
}
