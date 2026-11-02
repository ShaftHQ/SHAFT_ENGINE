"""Private banned terms for neutrality tests, loaded from an untracked local file.

Client and brand identifiers must never be committed. The file is read
from ``CE_NEUTRAL_TERMS_FILE`` or ``~/.config/chaos-engine/neutral-terms.txt``,
with one term per line (``#`` starts a comment). A missing file means
there are no extra terms.
"""

from __future__ import annotations

import os
from pathlib import Path


def private_terms() -> tuple[str, ...]:
    path = Path(os.environ.get("CE_NEUTRAL_TERMS_FILE", Path.home() / ".config/chaos-engine/neutral-terms.txt"))
    if not path.is_file():
        return ()
    lines = (line.split("#", 1)[0].strip().lower() for line in path.read_text(encoding="utf-8").splitlines())
    return tuple(line for line in lines if line)
