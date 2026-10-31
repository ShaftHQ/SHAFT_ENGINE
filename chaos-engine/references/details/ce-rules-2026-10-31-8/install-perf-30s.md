# PERF install ≤30 s

Carl: designer-workspace install 43 s > 30 s slow mark. `upgrade_session`
post-verify ran doctor twice even when already healthy. Fast-path: one doctor
when healthy; skip repair + second doctor. Receipts record `elapsed_s` and
`doctor_passes`. Slow mark: 30 s (`SLOW_INSTALL_MARK_S`).
