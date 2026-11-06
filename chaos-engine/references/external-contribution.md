# External contribution tracker

Rule only. Do not land an upstream patch, a product bump, or a release from this card.

Close a contribution tracker only after all three are true:

1. The upstream change is merged.
2. The upstream project has released the change.
3. The product repository has adopted that release.

While the upstream review is stale or `CHANGES_REQUESTED`, record that check before treating the tracker as waiting on us. A pull request opened from a fork asks for the fork label. Upstream text is English only.

Enforced by [`lessons_2026_11_06.py`](../rules/lessons_2026_11_06.py) (`evaluate_external_contribution`).
