---
name: diagram-design
description: Use when drawing a diagram (flow, architecture, process, relationship) for a still, slide, page or video frame.
---

# W05 Diagram design

## Use when

A diagram is drawn for any surface. It extends the diagram rule in
[image-graphics](../image-graphics.md).

## Rules

1. A table would do? Then don't draw: use the table.
2. Each node is one distinct idea; merge nodes that always co-travel.
3. Every edge carries information (a verb, a quantity, a direction); an
   edge that says nothing is removed.
4. Accent at most 2 focal elements; everything else is neutral.
5. Orthogonal edges with r8 rounded elbows; parallel edges stay >= 12px
   apart; several edges into one node fan out along its side.
6. Labels are <= 14 characters, with a 6-10px mask gap where a label
   crosses a line.
7. Complexity budget: past it, split into one overview plus detail views.
8. Removal pass: delete each element in turn; keep it only if meaning is
   lost.
9. SVG output has `role="img"` plus `<title>` and `<desc>`.
10. RTL: the flow direction mirrors (start at the inline-start side).

## Verify

- [ ] The node list has no co-travelling pair; every edge has a label or
      a stated meaning.
- [ ] At most 2 accent-coloured elements.
- [ ] The SVG passes a check for `role="img"`, `<title>` and `<desc>`.
- [ ] The RTL version flows from the right.

## Sources

- Adapted with attribution from
  [diagram-design](https://github.com/cathrynlavery/diagram-design) @0b9b10c
  by Cathryn Lavery, MIT. Nothing is vendored.
