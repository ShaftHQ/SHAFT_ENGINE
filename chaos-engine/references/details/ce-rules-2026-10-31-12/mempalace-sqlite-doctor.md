# L84 MEMPALACE-SQLITE-DOCTOR

`install.py doctor` must flag a missing `sqlite_exact.sqlite3` under the
project palace. Repair must not leave empty `mempalace.previous-*` directories.

Enforced by [`lessons_l82_l92.py`](../../../rules/lessons_l82_l92.py) (`evaluate_mempalace_sqlite`) with proven-red unit tests.
