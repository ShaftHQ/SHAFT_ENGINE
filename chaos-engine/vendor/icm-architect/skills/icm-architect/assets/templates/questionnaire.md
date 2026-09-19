# Setup questionnaire — configure the factory once

Answers get written into `_shared/` as reference files. Every future run reads them; no run should ever re-ask them.

1. Who is this workspace for, and what should a finished deliverable look like? (becomes `_shared/definition-of-done.md`)
2. Voice and tone: paste two examples of past work that sound right, and one that sounds wrong. (becomes `_shared/voice.md`)
3. Hard constraints that never bend: length, format, brand rules, compliance. (becomes `_shared/rules.md`)
4. What does the human always check before anything ships? (becomes each stage's Human check line)
5. What already exists that runs should reuse — templates, examples, data sources? (linked from `_shared/`, one home per fact)
