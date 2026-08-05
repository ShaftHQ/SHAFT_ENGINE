Three times in one session a guard was defined, tested green, and unreachable. The tests call check_rNN(...) directly, so they pass identically whether or not run_pretooluse or run_stop ever calls it. A rule that ships defined-but-unbound is worse than one that ships late: it reads as coverage on the PR and in the suite.

The three instances, each with a different mechanism:

1. R13 and R14 shipped unwired. The wiring step asserted on a dispatch anchor that no longer matched, the assertion failed, and the commit went ahead anyway because the shell steps were newline-separated rather than chained on success. A failing setup step does not stop a later `git commit` on its own line.
2. R16 passed its tests while incapable of ever firing, because nothing recorded the `commit` and `memory-write` events it reads from the session ledger. The gate was real, the input was never written, and the tests patched ledger_events so they never noticed.
3. `scripts/ci/validate_agent_setup.py` imported harness_reachability above the sys.path insert. All 51 tests passed because unittest runs from the repository root where `scripts` is already importable; the command died with ModuleNotFoundError from any other cwd, which is the only way an agent actually runs it.

A fourth variant, same family: after `git reset --hard` destroyed the source, the suite went green against a stale `__pycache__/*.pyc` for a module that no longer existed on disk. A passing run proved nothing because the artifact under test was gone.

What to do instead, in the same step that writes the guard:

- Assert the dispatch call site exists after editing, by reading the file back and checking for the literal call, not by trusting the edit's own return.
- Chain shell steps on success (`&&`) whenever a later step must not run if an earlier one failed.
- If the gate reads state, wire whatever records that state in the same commit, and prove the pair end to end.
- Exercise the real entry point -- pipe a JSON payload into `py -3 scripts/agents/guard.py`, run the CLI from an unrelated cwd -- not only the unit under test.
- Clear `__pycache__` before trusting any suite run taken after a file went missing or moved.

General shape: every one of these is a green signal independent of the thing it claimed to verify, which is the unbound-check pattern references/verification-gap-lens.md exists to name. Producing it four times while building enforcement machinery is the strongest available argument that the machinery is needed.