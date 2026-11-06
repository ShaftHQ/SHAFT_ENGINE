L7693 RUN-STATE

Persist active|frozen|halted; refuse priority dispatch when not active; FYI non-priority; owner channel = ack + result.
Check run-state before every dispatch. Token restore is not a resume (#7767).

Enforced by [`lessons_2026_11_02.py`](../../../rules/lessons_2026_11_02.py) (`evaluate_run_state`).
