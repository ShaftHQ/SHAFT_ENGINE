---
name: dataviz-integrity-print
description: Use when choosing or reviewing a chart, infographic data or a figure exported to print or PDF.
---

# G01 Data viz integrity and print

## Use when

A chart or figure is made for a page, slide, still or PDF. Diagrams are
[W05](../web-craft/diagram-design.md).

## Rules

1. **Chart by question:** trend = line; ranking = sorted horizontal bar;
   composition = stacked area or 100% bar; correlation = scatter.
2. **Never 3D;** pie only under 6 categories.
3. **Integrity:** scale area, not radius; never alter or hide data to
   improve a figure; colour is never the only cue; graphical objects 3:1.
4. **Print and PDF:** embed TrueType fonts (Type 42); size in mm at final
   size (an 89 mm column, say); `scale` is not DPI; WebGL traces
   rasterise in PDF, so force SVG mode.

## Verify

- [ ] The chart type is named against the question it answers.
- [ ] `pdffonts figure.pdf` lists every font as embedded, none Type 3.
- [ ] `design_qc.py contrast` on series colours against the background (3:1).

## Sources

- [anthropics/knowledge-work-plugins data-visualization](https://github.com/anthropics/knowledge-work-plugins/tree/33d3dea41a341b341d4f5da2e8801ae0c89be2b8)
  @ `33d3dea41a341b341d4f5da2e8801ae0c89be2b8` (Apache-2.0, adapted).
- [k-dense-ai/scientific-agent-skills scientific-visualization](https://github.com/k-dense-ai/scientific-agent-skills/tree/593377bb048752abb0152deab9fbb1083a6087df)
  @ `593377bb048752abb0152deab9fbb1083a6087df` (MIT, adapted).
