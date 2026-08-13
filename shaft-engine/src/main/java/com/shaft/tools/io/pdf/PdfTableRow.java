package com.shaft.tools.io.pdf;

import java.util.List;
import java.util.Objects;

/** One immutable inferred PDF table row. */
public record PdfTableRow(List<PdfTableCell> cells) {
    public PdfTableRow {
        cells = List.copyOf(Objects.requireNonNull(cells, "cells"));
    }
}
