# Hook trigger map

Each ChaosEngine lifecycle event fires only at its logical point. Must-hold
rules are PreToolUse deny. PostToolUse stays silent unless the text changes the
next decision. SessionStart is guide-only context (Grok ignores SessionStart
stdout); standing instructions live in the project router, not SessionStart.

| Event | Hosts | Mode | Next-decision effect |
| --- | --- | --- | --- |
| `SessionStart` | Claude, Codex, Gemini, Grok, Copilot | guide | Inject compact locators and session identity. Not enforcement. Grok ignores SessionStart stdout — compensate via entrypoint load. |
| `UserPromptSubmit` | Claude, Codex, Gemini, Grok, Copilot | guide | Companion mode tracking only when the host supports it. |
| `PreToolUse` | Claude, Codex, Gemini, Grok, Copilot | enforce | Deny catastrophic or out-of-contract tool use; hold work that owes a reflection receipt; deny primary-checkout mutations when a session worktree exists. |
| `PostToolUse` | Claude, Codex, Gemini, Grok, Copilot | guide | Record mutation/delivery for reflection. Stay silent unless stdout changes the next decision. |
| `PostToolUseFailure` | Claude, Codex, Grok, Copilot | guide | Record failure and inject a pending reflection checkpoint only when one is owed. |
| `Stop` | Claude, Codex, Gemini, Grok, Copilot | enforce | Collect incomplete delivery duties; require the root Learning Session after delivery. |
| `SubagentStop` | Claude, Codex, Grok, Copilot | enforce | Delegate-owned completion duties only. Never start the root Learning Session. |
| `PreCompact` | Claude, Gemini, Copilot | guide | Re-inject compact locators that compaction would drop. |
| `SessionEnd` | Claude, Codex, Gemini, Grok, Copilot | enforce | Remove this session's worktree after merge is recorded and the tree is clean. |

Hosts without a hook primitive cannot enforce this table; say so in the install
receipt. Narrow matchers live in `hooks/matchers.json`.
