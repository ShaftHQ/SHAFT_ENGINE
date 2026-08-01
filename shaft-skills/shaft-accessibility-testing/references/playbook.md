# Accessibility-testing playbook

## Ten practices

1. Declare the target platform, WCAG version, conformance level, included content, and supported assistive technologies. [`W3C-WCAG22`]
2. Cover full pages and complete critical flows, including responsive variants, errors, overlays, authentication, and state changes. [`W3C-WCAG22`]
3. Combine automated rules with keyboard, visual, screen-reader, zoom, and user-centered manual evaluation. [`W3C-ACT`, `W3C-WCAG22`]
4. Verify keyboard access, logical focus order, visible focus, focus containment, escape, and focus restoration. [`W3C-WCAG22`]
5. Verify names, roles, values, labels, headings, landmarks, relationships, state announcements, and semantic alternatives. [`W3C-WCAG22`, `W3C-ACT`]
6. Verify contrast, color independence, reflow, text spacing, resize, orientation, and content visibility. [`W3C-WCAG22`]
7. Verify text alternatives, captions, transcripts, audio control, and meaningful media behavior where applicable. [`W3C-WCAG22`]
8. Verify instructions, input purpose, error identification, suggestions, prevention, status messages, and authentication accessibility. [`W3C-WCAG22`]
9. Verify target size, pointer alternatives, drag alternatives, motion control, time limits, flashing, and mobile accessibility semantics. [`W3C-WCAG22`, `ANDROID-TESTING`]
10. Report each finding with criterion, level, page or state, steps, evidence, affected users, severity, remediation owner, and retest result. [`W3C-WCAG22`, `ISTQB-TM`]

## Examples

- Test a checkout form with keyboard-only input, screen reader, zoom, error recovery, status announcements, and contrast checks.
- Test a modal for focus entry, containment, visible order, escape, background isolation, and focus restoration.
- Test a native mobile login screen for content descriptions, traversal order, touch targets, dynamic text, orientation, and error announcements.

## Boundary case

- An automated scan with zero findings does not prove WCAG conformance; report only the rules and states actually checked and require the missing manual coverage.

## Output

Return scope and level, test matrix, criterion-linked findings, affected users,
evidence, severity, limitations, remediation, and retest status.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
