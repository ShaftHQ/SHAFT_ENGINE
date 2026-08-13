package com.shaft.tools.io.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import com.shaft.tools.io.pdf.PdfTable;
import com.shaft.tools.io.pdf.PdfTableCell;
import com.shaft.tools.io.pdf.PdfTableRow;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

final class PdfTableExtractor {
    private PdfTableExtractor() {
        throw new IllegalStateException("Utility class");
    }

    static Extraction extract(OcrResult result, double threshold) {
        List<OcrTextBlock> words = result.blocks().stream().filter(block -> block.level() == OcrBlockLevel.WORD)
                .sorted(Comparator.comparingInt((OcrTextBlock block) -> block.bounds().y())
                        .thenComparingInt(block -> block.bounds().x())).toList();
        List<List<OcrTextBlock>> rows = groupRows(words);
        List<List<OcrTextBlock>> candidates = rows.stream().filter(row -> row.size() >= 2).toList();
        if (candidates.size() < 2) {
            return new Extraction(List.of(), List.of());
        }
        int commonColumns = candidates.stream().mapToInt(List::size).min().orElse(0);
        if (commonColumns < 2 || candidates.stream().filter(row -> row.size() == commonColumns).count() < 2) {
            return new Extraction(List.of(), List.of("Possible table structure was below the required consistency."));
        }
        List<List<OcrTextBlock>> normalized = candidates.stream().filter(row -> row.size() == commonColumns).toList();
        double alignment = columnAlignment(normalized, commonColumns);
        if (alignment < 0.75 || !hasColumnWhitespace(normalized, commonColumns)) {
            return new Extraction(List.of(), List.of("Aligned prose did not have stable table column whitespace."));
        }
        double confidence = Math.min(0.95, 0.55 + normalized.size() * 0.05 + alignment * 0.25);
        if (confidence < threshold) {
            return new Extraction(List.of(), List.of("Possible table structure confidence " + confidence
                    + " was below " + threshold + "."));
        }
        List<PdfTableRow> tableRows = normalized.stream().map(row -> new PdfTableRow(row.stream()
                .map(word -> new PdfTableCell(word.text(), word.bounds(), 1, 1,
                        Math.min(word.confidence(), confidence))).toList())).toList();
        List<OcrTextBlock> all = normalized.stream().flatMap(List::stream).toList();
        return new Extraction(List.of(new PdfTable(PdfNativeTextExtractor.union(all), tableRows, confidence)), List.of());
    }

    private static List<List<OcrTextBlock>> groupRows(List<OcrTextBlock> words) {
        List<List<OcrTextBlock>> rows = new ArrayList<>();
        for (OcrTextBlock word : words) {
            List<OcrTextBlock> row = rows.isEmpty() ? null : rows.getLast();
            if (row == null || Math.abs(row.getFirst().bounds().y() - word.bounds().y())
                    > Math.max(4, word.bounds().height() / 2)) {
                row = new ArrayList<>();
                rows.add(row);
            }
            row.add(word);
        }
        return rows;
    }

    private static double columnAlignment(List<List<OcrTextBlock>> rows, int columns) {
        double score = 0;
        for (int column = 0; column < columns; column++) {
            int min = Integer.MAX_VALUE;
            int max = Integer.MIN_VALUE;
            double averageWidth = 0;
            for (List<OcrTextBlock> row : rows) {
                OcrTextBlock word = row.get(column);
                min = Math.min(min, word.bounds().x());
                max = Math.max(max, word.bounds().x());
                averageWidth += word.bounds().width();
            }
            averageWidth /= rows.size();
            score += Math.max(0, 1 - (max - min) / Math.max(1, averageWidth));
        }
        return score / columns;
    }

    private static boolean hasColumnWhitespace(List<List<OcrTextBlock>> rows, int columns) {
        for (int column = 0; column < columns - 1; column++) {
            for (List<OcrTextBlock> row : rows) {
                int gap = row.get(column + 1).bounds().x() - row.get(column).bounds().right();
                int minimum = Math.max(row.get(column).bounds().height(), row.get(column + 1).bounds().height());
                if (gap < minimum) {
                    return false;
                }
            }
        }
        return true;
    }

    record Extraction(List<PdfTable> tables, List<String> warnings) {
    }
}
