# L72 design fix-loop (localized after cap-2)

After **two** failed full-regen rounds, the next round is a **localized masked
edit / composite** of the failing region onto the last accepted candidate
(protect passing metrics). No more full regen by default. Prefer localized edit
before another full generation.

Module: [`design_fix_loop.py`](../../../rules/design_fix_loop.py).
