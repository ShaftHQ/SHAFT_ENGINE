# Rule and lesson retirement

Per-rule trigger counters live in `.chaos-engine-state/rule-triggers.json`.
Each record is a rule id, a count, and `lastTriggered`. The file stores no
prompt text, no document content, and no path.

Record a trigger with `record_rule_trigger(project, rule_id)` in
[harness_mindset.py](../harness_mindset.py). The ids are `disjoint-trees`,
`rg-explicit-path`, `relevance-floor`, `retrieve-usage`, `host-prune`,
`memory-no-hit`, `riskiest-assumption`, `plan-receipts`, `pattern-before-triage`,
`lesson-outcome`, and `rule-retirement`.

## Monthly review

Once a month, run:

```bash
python3 -c "import importlib.util; p='chaos-engine/harness_mindset.py'; s=importlib.util.spec_from_file_location('m', p); m=importlib.util.module_from_spec(s); s.loader.exec_module(m); print(m.zero_trigger_report(__import__('pathlib').Path('.')))"
```

The report lists every catalog rule with zero triggers inside the 30-day
window. A missing counter counts as zero.

## Retirement

For each id on that list, either retire the rule in the same change that
updates this document, or add one sentence here that justifies keeping it.
Do not retire a rule from the baseline report below: the counter starts with
this change, so an empty window is not evidence the rule is dead.

## First report

Window: 30 days. Baseline: yes (no triggers recorded yet).

Zero triggers:

- disjoint-trees
- rg-explicit-path
- relevance-floor
- retrieve-usage
- host-prune
- memory-no-hit
- riskiest-assumption
- plan-receipts
- pattern-before-triage
- lesson-outcome
- rule-retirement

Justification for keeping every id on this baseline: the counter was added in
the same change as this report, so retire or justify again after one full
window of recorded triggers.
