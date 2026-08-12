package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrMatch;
import com.shaft.gui.ocr.OcrMatchMode;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.gui.ocr.OcrTextBlock;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

final class OcrTargetResolver {
    private static final Comparator<OcrTextBlock> READING_ORDER = Comparator
            .comparingInt((OcrTextBlock block) -> block.bounds().y())
            .thenComparingInt(block -> block.bounds().x());

    private OcrTargetResolver() {
    }

    static OcrMatch resolve(OcrResult result, OcrTarget target) {
        List<OcrTextBlock> lines = result.blocks().stream()
                .filter(block -> block.level() == OcrBlockLevel.LINE)
                .filter(block -> block.confidence() >= target.options().minimumConfidence())
                .sorted(READING_ORDER)
                .toList();
        if (lines.isEmpty()) {
            lines = result.blocks().stream()
                    .filter(block -> block.level() == OcrBlockLevel.WORD)
                    .filter(block -> block.confidence() >= target.options().minimumConfidence())
                    .sorted(READING_ORDER)
                    .toList();
        }

        List<OcrMatch> matches = new ArrayList<>();
        for (OcrTextBlock line : lines) {
            String observed = normalize(line.text(), target);
            String expected = normalize(target.expectedText(), target);
            if (target.matchMode() == OcrMatchMode.EXACT && observed.equals(expected)) {
                matches.add(new OcrMatch(normalizeWhitespace(line.text()), line.bounds(), line.confidence()));
            } else if (target.matchMode() == OcrMatchMode.CONTAINS && observed.contains(expected)) {
                matches.addAll(narrowToWords(result.blocks(), line, expected, target));
            }
        }

        if (matches.isEmpty()) {
            throw new IllegalStateException("No OCR match found for '" + target.expectedText()
                    + "' at minimum confidence " + target.options().minimumConfidence() + ".");
        }
        if (target.requireUniqueMatch()) {
            if (matches.size() != 1) {
                throw new IllegalStateException("Expected one OCR match for '" + target.expectedText() + "' but found "
                        + matches.size() + " OCR matches: " + matches);
            }
            return matches.getFirst();
        }
        if (target.occurrence() >= matches.size()) {
            throw new IllegalStateException("OCR occurrence " + target.occurrence() + " was requested for '"
                    + target.expectedText() + "' but only " + matches.size() + " matches were found.");
        }
        return matches.get(target.occurrence());
    }

    private static List<OcrMatch> narrowToWords(List<OcrTextBlock> allBlocks, OcrTextBlock line,
                                                String normalizedExpected, OcrTarget target) {
        List<OcrTextBlock> words = allBlocks.stream()
                .filter(block -> block.level() == OcrBlockLevel.WORD)
                .filter(block -> block.confidence() >= target.options().minimumConfidence())
                .filter(block -> containedBy(block.bounds(), line.bounds()))
                .sorted(Comparator.comparingInt(block -> block.bounds().x()))
                .toList();
        if (words.isEmpty()) {
            return List.of(new OcrMatch(normalizeWhitespace(line.text()), line.bounds(), line.confidence()));
        }

        StringBuilder joined = new StringBuilder();
        List<Integer> starts = new ArrayList<>();
        List<Integer> ends = new ArrayList<>();
        for (OcrTextBlock word : words) {
            if (!joined.isEmpty()) {
                joined.append(' ');
            }
            starts.add(joined.length());
            joined.append(normalizeWhitespace(word.text()));
            ends.add(joined.length());
        }
        String comparableWords = target.options().caseSensitive()
                ? joined.toString()
                : joined.toString().toLowerCase(Locale.ROOT);
        List<OcrMatch> narrowed = new ArrayList<>();
        int searchFrom = 0;
        while (searchFrom <= comparableWords.length() - normalizedExpected.length()) {
            int matchStart = comparableWords.indexOf(normalizedExpected, searchFrom);
            if (matchStart < 0) {
                break;
            }
            int matchEnd = matchStart + normalizedExpected.length();
            List<OcrTextBlock> selectedWords = new ArrayList<>();
            for (int index = 0; index < words.size(); index++) {
                if (starts.get(index) < matchEnd && ends.get(index) > matchStart) {
                    selectedWords.add(words.get(index));
                }
            }
            OcrRectangle bounds = selectedWords.stream().map(OcrTextBlock::bounds).reduce(OcrRectangle::union).orElse(line.bounds());
            double confidence = selectedWords.stream().mapToDouble(OcrTextBlock::confidence).min().orElse(line.confidence());
            String text = String.join(" ", selectedWords.stream().map(OcrTextBlock::text).map(OcrTargetResolver::normalizeWhitespace).toList());
            narrowed.add(new OcrMatch(text, bounds, confidence));
            searchFrom = matchEnd;
        }
        return narrowed.isEmpty()
                ? List.of(new OcrMatch(normalizeWhitespace(line.text()), line.bounds(), line.confidence()))
                : List.copyOf(narrowed);
    }

    private static boolean containedBy(OcrRectangle child, OcrRectangle parent) {
        int childCenterX = child.centerX();
        int childCenterY = child.centerY();
        return childCenterX >= parent.x() && childCenterX <= parent.right()
                && childCenterY >= parent.y() && childCenterY <= parent.bottom();
    }

    private static String normalize(String text, OcrTarget target) {
        String normalized = target.options().normalizeWhitespace() ? normalizeWhitespace(text) : text;
        return target.options().caseSensitive() ? normalized : normalized.toLowerCase(Locale.ROOT);
    }

    private static String normalizeWhitespace(String text) {
        return text == null ? "" : text.strip().replaceAll("\\s+", " ");
    }
}
