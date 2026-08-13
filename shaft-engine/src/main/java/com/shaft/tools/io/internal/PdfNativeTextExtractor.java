package com.shaft.tools.io.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.apache.pdfbox.text.TextPosition;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

final class PdfNativeTextExtractor extends PDFTextStripper {
    private final double scale;
    private final List<OcrTextBlock> words = new ArrayList<>();

    PdfNativeTextExtractor(int pageNumber, int renderDpi) throws IOException {
        scale = renderDpi / 72.0;
        setStartPage(pageNumber);
        setEndPage(pageNumber);
        setSortByPosition(true);
    }

    OcrResult extract(PDDocument document, int width, int height) throws IOException {
        String text = getText(document).strip();
        List<OcrTextBlock> blocks = new ArrayList<>();
        if (!text.isBlank()) {
            blocks.add(new OcrTextBlock(text, new OcrRectangle(0, 0, width, height), 1, OcrBlockLevel.PAGE));
            blocks.add(new OcrTextBlock(text, union(words), 1, OcrBlockLevel.BLOCK));
            blocks.add(new OcrTextBlock(text, union(words), 1, OcrBlockLevel.PARAGRAPH));
        }
        blocks.addAll(lines(words));
        blocks.addAll(words);
        return new OcrResult(text, blocks);
    }

    @Override
    protected void writeString(String text, List<TextPosition> positions) throws IOException {
        StringBuilder word = new StringBuilder();
        List<TextPosition> wordPositions = new ArrayList<>();
        for (TextPosition position : positions) {
            String value = position.getUnicode();
            if (value == null || value.isBlank()) {
                emitWord(word, wordPositions);
            } else {
                word.append(value);
                wordPositions.add(position);
            }
        }
        emitWord(word, wordPositions);
        super.writeString(text, positions);
    }

    private void emitWord(StringBuilder text, List<TextPosition> positions) {
        if (text.isEmpty() || positions.isEmpty()) {
            text.setLength(0);
            positions.clear();
            return;
        }
        int left = Integer.MAX_VALUE;
        int top = Integer.MAX_VALUE;
        int right = 0;
        int bottom = 0;
        for (TextPosition position : positions) {
            int x = scaled(position.getXDirAdj());
            int y = scaled(position.getYDirAdj() - position.getHeightDir());
            left = Math.min(left, x);
            top = Math.min(top, y);
            right = Math.max(right, x + Math.max(1, scaled(position.getWidthDirAdj())));
            bottom = Math.max(bottom, y + Math.max(1, scaled(position.getHeightDir())));
        }
        words.add(new OcrTextBlock(text.toString(), new OcrRectangle(Math.max(0, left), Math.max(0, top),
                Math.max(1, right - left), Math.max(1, bottom - top)), 1, OcrBlockLevel.WORD));
        text.setLength(0);
        positions.clear();
    }

    private int scaled(float value) {
        return (int) Math.round(value * scale);
    }

    private static List<OcrTextBlock> lines(List<OcrTextBlock> input) {
        List<OcrTextBlock> ordered = input.stream().sorted(Comparator
                .comparingInt((OcrTextBlock block) -> block.bounds().y())
                .thenComparingInt(block -> block.bounds().x())).toList();
        List<List<OcrTextBlock>> groups = new ArrayList<>();
        for (OcrTextBlock word : ordered) {
            List<OcrTextBlock> group = groups.isEmpty() ? null : groups.getLast();
            if (group == null || Math.abs(group.getFirst().bounds().y() - word.bounds().y())
                    > Math.max(3, word.bounds().height() / 2)) {
                group = new ArrayList<>();
                groups.add(group);
            }
            group.add(word);
        }
        return groups.stream().map(group -> new OcrTextBlock(group.stream().map(OcrTextBlock::text)
                        .reduce((left, right) -> left + " " + right).orElse(""), union(group), 1,
                OcrBlockLevel.LINE)).toList();
    }

    static OcrRectangle union(List<OcrTextBlock> blocks) {
        if (blocks.isEmpty()) {
            return new OcrRectangle(0, 0, 1, 1);
        }
        int left = blocks.stream().mapToInt(block -> block.bounds().x()).min().orElse(0);
        int top = blocks.stream().mapToInt(block -> block.bounds().y()).min().orElse(0);
        int right = blocks.stream().mapToInt(block -> block.bounds().x() + block.bounds().width()).max().orElse(1);
        int bottom = blocks.stream().mapToInt(block -> block.bounds().y() + block.bounds().height()).max().orElse(1);
        return new OcrRectangle(left, top, Math.max(1, right - left), Math.max(1, bottom - top));
    }
}
