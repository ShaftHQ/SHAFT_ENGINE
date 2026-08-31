from pathlib import Path

root = Path(__file__).resolve().parent
observed = (root / "source.txt").read_text().strip()
if observed != "OPENAI_API_KEY=[REDACTED]\nFAIL_CODE=E42":
    raise SystemExit("result bundle is unsafe or lost its diagnostic")
print(observed)
