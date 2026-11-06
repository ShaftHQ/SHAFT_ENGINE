L7709 ADVERSARIAL-PASS-SHA

Adversarial PASS is SHA-bound; post-review commits require re-ack before merge/enqueue.
Two independent verdicts at the head SHA. When the external CLI fails, record the fallback reason. The designer does not review their own work.

Enforced by [`lessons_2026_11_02.py`](../../../rules/lessons_2026_11_02.py) (`evaluate_adversarial_pass_sha`).
