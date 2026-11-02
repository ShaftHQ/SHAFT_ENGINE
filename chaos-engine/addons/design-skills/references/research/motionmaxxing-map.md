# motionmaxxing study: rule map

Source: [Tejashmakwana/motionmaxxing](https://github.com/Tejashmakwana/motionmaxxing)
at commit `8c8ec0f2a6f6c9a0da15cd24f7b1298ab298368c`, Apache-2.0, by Tejas Makwana.
Ideas are re-expressed in our own words; nothing is copied verbatim, and no
bundled GSAP, font, or showcase media is used (its NOTICE excludes them from
Apache-2.0). Line refs are `SKILL.md:L` or `motion.md:L` at that commit.

Status: **adopt** (taken as a rule with a check), **adapt** (changed to fit
explainers and READMEs), **already have**, **reject** (with the reason).

| Rule | Source | Status | Our file / check |
| --- | --- | --- | --- |
| No page chrome: corner labels, counters, timecodes, tracked caps, progress bars | SKILL.md:30, :69 (G5) | adopt | [D09](../html-motion-graphics.md), `design_qc.py chrome` |
| No slogan copy: one-word triplets, "Not just X", "Unlock", "Meet X", "the future of" | SKILL.md:35 | adopt | [D22](../launch-video.md), `copyslop` |
| Entries move; opacity is a 4-6 f helper, never the whole entry | motion.md:73 (R1.6), SKILL.md:50 | adopt | [D08](../motion-principles.md), `fadeonly` |
| At most one visible spring per film; overshoot is rare, late, small | motion.md:96-97 (R1.10-11), SKILL.md:49 | adopt | D08, `springs --max 1` |
| Default move about 10-12 f (0.33-0.4 s); 20-30 f ease-outs everywhere is too slow | motion.md:27 (R1.1) | adopt | D08, `motiondur --max 0.6` |
| End hold 0.6-1.4 s; final shot at most 25% of the film | SKILL.md:67 (G3) | adapt: README or CTA cards may pass `--max` up to 4 s when the owner asks for a held button | [D20](../delivery-qc.md), `endhold` |
| Human films sit in a motion band (mean luma diff median 6.8, IQR 4.4-9.8) | motion.md (sec 1.7), SKILL.md:52 | adapt: a gate band 0.5-6 on our 64x36 luma metric (their own comparison films measure 0.7-3.1 on it), not a question | D20, `motionband` |
| No run of more than 3 flat frames | SKILL.md:66 (G2) | already have | `flatframes` |
| Land decelerates, leave accelerates | SKILL.md:49, motion.md R1.4-1.5 | already have (D08 easing table); strengthened | D08, `easing` |
| Every beat buys one belief ("Buys ___") | SKILL.md:45 | adopt as storyboard column | [D01](../design-brief-storyboard.md), `brief` |
| Show it, then say it: claim then hard cut to the product doing it | SKILL.md:46 | already have (real evidence iron rule 4) | SKILL.md rule 4 |
| Shots hand off: last frame of N is first frame of N+1 | SKILL.md:47 | adopt | [craft/transitions](../craft/transitions.md) |
| Cut on motion, never on rest | SKILL.md:48 | adopt | [craft/video-editing](../craft/video-editing.md), `cutrhythm` |
| Arrival from oversize, nothing grows from zero | SKILL.md:51 | adopt | [craft/motion-graphics-craft](../craft/motion-graphics-craft.md) |
| One system per film: one clock, one blur grammar, one accent meaning | SKILL.md:53 | adopt | craft/motion-graphics-craft |
| One focal point per frame, at most 3 | SKILL.md:54 | adopt (review question, not scriptable) | craft/graphic-design rubric |
| Words 2-12 f apart; letter typing only in the product's own input | SKILL.md:55 | adopt | craft/motion-graphics-craft |
| Physical demo: cursor parks, press squashes 2-3 f | SKILL.md:56 | adapt for real captures: zoom and click emphasis only | [D11](../screen-capture.md) |
| Write at least three ideas of different kinds, then pick one | SKILL.md:90 | adopt | [runbook](../pipelines/runbook.md) stage 0 |
| Headlines 7-11% of H; hooks 3-6 s; cuts every 0.3-2.3 s | SKILL.md:134-135 | adopt as targets | craft/hooks, craft/video-editing |
| Never ship invented stats or a stat slide | SKILL.md:34 | already have (real evidence) | SKILL.md rule 4 |
| Web-footer ending (logo + tagline + button + URL) | SKILL.md:39 | adapt: a README end card needs a CTA; keep it one action, held per brief | craft/hooks |
| No deck layouts (kicker above headline, headline left / device right) | SKILL.md:31 | adopt as review question | craft/graphic-design |
| A held frame keeps one living thing | SKILL.md:52 | already have (`static`, `contenthold`) | D13 |
| Self-certification banned; quote script numbers | SKILL.md:40-41 | already have (verified-findings gate) | runbook stage 4 |
| Brand capture by URL scraping, ElevenLabs voice and music | SKILL.md steps 1, 5, 8 | reject: needs paid keys and third-party scraping; our pipeline uses owned captures and a manifest | D02, D17 |
| GSAP runtime and 3D hero kit | runtime/ | reject: GSAP is not open source (NOTICE); our renders stay plain HTML/CSS | INVENTORY.md |
| User taste files (`taste/verdicts.md`) | SKILL.md step 0 | adapt: the owner's verdicts live in project memory, not the add-on | core self-improve |
