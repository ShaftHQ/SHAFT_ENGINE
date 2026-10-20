# Each slide

## 4 One idea

- One idea per slide; slide count is free. Split "Title *continued*" chains
  into separate claims.
- Glance test: the point is clear in about 3 seconds.
- On-slide text: about 25 words for beginners, 40 for experts. The rest goes
  in the speaker notes. Slides are not documents; a reading copy is a
  slidedoc (hidden appendix).

## 5 Assertion-evidence

- Title is the claim: a full sentence, left-aligned, at most 2 lines. The old
  topic name becomes a small kicker above it.
- Body is evidence: diagram, screenshot, code, table, chart, big number, or
  comparison. At most one list of 2-4 items.
- Draw the relationship the words name: stages → process, "vs" → two columns,
  "over time" → timeline.

## 6 Multimedia principles

- Coherence: cut decoration that carries no meaning. Repeated stock tiles go.
- Signaling: one accent colour per slide shows where to look.
- Redundancy and modality: do not put the spoken sentences on the slide;
  never read the slide aloud.
- Spatial contiguity: labels on the diagram, no legend hunting. Temporal
  contiguity: show the picture while you say it.
- Speaker notes carry talking points, moved original text, and a source URL
  for every fact.
- Builds: Appear or Fade only, 0.3 s, on click, at most 5 per slide, for list items,
  cards, or myth-to-fact pairs. Never animate titles, footers, logos, or
  charts. The fully built slide must read alone (PDF and print show it). One
  transition style deck-wide.
- Google Slides import: a build on a group holding one shape is dropped, so
  target the shape. An object build inside a paragraph-by-paragraph list is
  moved out and adds a click, so put bullets and objects in separate boxes.

## 6a Media treatment

Sort every visual before placing it; one deck, one treatment per kind.

| Kind | Treatment |
| --- | --- |
| Real screenshot, GIF, video frame | Original colours, never tinted (it must stay truthful). One frame style; caption `Screenshot: <tool> <version or date>, source`. Recapture if the UI changed. |
| Generic or vendor diagram, chart | Redraw natively in palette roles; never paste it. |
| Legacy image with baked-in text | Rebuild as native shapes and text when its slide is touched. |
| Decorative photo, AI illustration | Duotone to the palette, or cut out; no raw stock colour. |
| Meme | Original image, framed; takeaway as text beside it. |
| Logo wall | Greyscale, equal cells, equal-area sizing (`w = sqrt(A*aspect)`, `h = sqrt(A/aspect)`, padding trimmed). Only organisations the presenter's CV backs. |
| Icon, symbol | One outline set recoloured by role; never a glyph (see card 9). |

No screenshot without an account: use the vendor's official docs image with
its URL, else a native mock labelled "Illustration, not a product screenshot".
