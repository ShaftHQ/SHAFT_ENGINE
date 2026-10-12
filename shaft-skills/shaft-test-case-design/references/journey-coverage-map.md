# Journey and coverage map

Use this when a design spans more than one source, such as a design catalog, a plan, and the app source. Do not invent a journey, a scenario, a step, an endpoint, or a gap. Every item traces to a source. A gap stays unsourced and names the catalogs that disagree.

## Hierarchy

A map is a journey, then a scenario, then a numbered step.

Each journey, scenario, and numbered step records these source-trace fields:

- Preconditions
- Test data
- Platforms
- API endpoints
- Source refs
- Dependencies

A numbered step also records its action, its expected result, and its side-effect class.

## Coverage matrix

| Plan service | Journey | Android | iOS | Design | Gap |
| --- | --- | --- | --- | --- | --- |
| source id | source id | present or absent, with a source ref | present or absent, with a source ref | present or absent, with a source ref | in code, not in plan; in plan, not in design; or none |

A cell is present only when a source ref says so. Leave "in code, not in plan" and "in plan, not in design" as gaps. Do not fill a gap with a plausible step.

## Reconciliation

| Design journey | Plan journey | Source that settles it | Ambiguous reason |
| --- | --- | --- | --- |
| catalog id | plan id | file or row | empty, or why the sources do not settle the mapping |

Add a reconciliation row when two catalogs number journeys differently, or when the plan puts a service in a different journey than the design. Keep an ambiguous mapping inline with its reason. Do not pick a number the sources do not give.

## Side-effect review

Classify each numbered step by screen state, not by control id or keyword. Record the reason on the step. A control id such as `submitBtn` is often the generic Continue on every wizard step, so a keyword rule flags safe Continues and misses a review screen whose action text has none of those words.

| Class | Meaning |
| --- | --- |
| SAFE | The step does not write, pay, or send. Automate it. |
| STOP-BEFORE | The screen is a final review, payment, cancel, reopen, reply, edit, OTP, upload, or feedback. Fill and assert the control is enabled. Do not tap, submit, pay, or send. |
| DESIGN-ONLY | A success or confirmation state. Check it from the design or from existing data. Do not drive the product into that state. |

Review the classified list once per journey before any automation.

## Scenario tracking

Exactly one status per row. The status set is Done, In progress, Blocked, and Not started.

- Done needs a passing run with platform, language, and date.
- Blocked names the defect id, or says "by design" for a production-writing scenario.
- In progress names an owner and the next step.
- Not started has no run yet.

Each scenario row and each journey row records Owner, Last updated, and Comments.

### Roll-up

Totals per status. Coverage percent with by-design rows included, and coverage percent with by-design rows excluded.

### Who updates it and when

The same pull request that changes the test updates the row. A retro updates rows whose runs landed outside that pull request.

## Mermaid journey diagrams

Draw one flowchart per journey: an entry node, then scenario nodes coloured by status, then step nodes. Draw a STOP-BEFORE step in a distinct shape or class. Collapse a long scenario to "+n more".

Sanitise a label before it enters the diagram. Remove or escape quotes, brackets, backticks, and the characters `#;|`.

Validate with `mmdc` from `@mermaid-js/mermaid-cli`. Run the command in a cache directory outside the repository, with a puppeteer config that sets `--no-sandbox`:

```
npx --yes @mermaid-js/mermaid-cli mmdc -i journey.mmd -o journey.svg -p puppeteer-no-sandbox.json
```

The puppeteer config contains `"args": ["--no-sandbox"]`. A diagram that does not render is not committed.
